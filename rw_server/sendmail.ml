open Types
open Types_conf

let (|>) x f = f x

exception SMTP_error of int * string list

let ssl_already_init = ref false

type socket =
  | Unix_socket of Unix.file_descr
  | SSL_socket of Ssl.socket


let create_message conf_e subject body attachment =
  (* This avoids to ask for the recipients's real name in the config file *)
  let recipients =
    List.map (fun address ->
      ("", address))
      conf_e.e_recipients
  in
  let b = Buffer.create 1024 in
  let attachments =
    match attachment with
      | None -> []
      | Some f -> [ Netsendmail.wrap_attachment
                      ~content_type:("application/octet-stream", [])
                      ~content_disposition:("attachment",
                        ["filename", Filename.basename f |> Mimestring.mk_param])
                      (new Netmime.file_mime_body f) ] in

    Netsendmail.compose ~from_addr:(conf_e.e_sender_name, conf_e.e_sender_address)
      ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8
      ~to_addrs:recipients ~subject:subject ~attachments:attachments body |>
        Netmime.write_mime_message (new Netchannels.output_buffer b);
        Buffer.contents b

let resolve name =
  try Unix.inet_addr_of_string name
  with Failure _ ->
        let h = Unix.gethostbyname name in
          h.Unix.h_addr_list.(0)

let socket_connect host port =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect s (Unix.ADDR_INET((resolve host), port));
    s

class smtp_client hostname port =
  object(self)
    val mutable channel = None
    val crlf_regexp = Str.regexp "\r\n"
    val new_line_regexp = Str.regexp "\\(\r\n\\|\r\\|\n\\)"

    method private get_channel =
      match channel with
	| None -> assert false
	| Some c' -> c'

    method private input_line =
      let input = match self#get_channel with
        | Unix_socket s -> Unix.read s
        | SSL_socket s -> Ssl.read s in
      let s = String.create 1 and b = Buffer.create 80 in
        input s 0 1 |> ignore;
        while s.[0] <> '\n' do
          Buffer.add_char b s.[0];
          try input s 0 1 |> ignore
          with End_of_file -> s.[0] <- '\n'
        done;
        Buffer.contents b

    method private output_string s =
      let output = match self#get_channel with
        | Unix_socket s -> Unix.write s
        | SSL_socket s -> Ssl.write s in
      let really_output s pos len =
        let rec print_rest n =
          if n < len
          then
            let m = output s (pos+n) (len-n) in
              print_rest (n+m)
          else
            () in
      print_rest 0 in
        really_output s 0 (String.length s)

    method private handle_reply =
      let rec read acc =
        let l = self#input_line in
	let msg = Printf.sprintf "S: %s%!" l in
	Log.log (msg, Normal_Extra);
          if l.[3] = '-' then read (l::acc)
          else int_of_string (String.sub l 0 3) , List.rev (l::acc) in
      let code, msg = read [] in
        match code/100 with
          | 2 | 3 -> ()
          | _ -> raise (SMTP_error (code, msg))

    method private smtp_cmd cmd =
      let msg = Printf.sprintf "C: %s%!" cmd in
      Log.log (msg, Normal_Extra);
      self#output_string cmd;
      self#output_string "\r\n";
      self#handle_reply

    method connect =
      channel <- Some (Unix_socket (socket_connect hostname port));
      self#handle_reply

    method ehlo =
      self#smtp_cmd ("EHLO " ^ (Unix.gethostname ()))

    method starttls =
      self#smtp_cmd "STARTTLS";
        let ssl_context = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
        let s = match self#get_channel with
                  | Unix_socket s -> s
                  | SSL_socket _ -> assert false in (* TODO *)
        let ssl_s = Ssl.embed_socket s ssl_context in
          Ssl.connect ssl_s;
          channel <- Some (SSL_socket ssl_s);

    method login user password =
      let encoded_login =
        Netencoding.Base64.encode
          (Printf.sprintf "%s\000%s\000%s" user user password) in
        self#smtp_cmd ("AUTH PLAIN " ^ encoded_login)

    method mail addr = self#smtp_cmd (Printf.sprintf "MAIL FROM:<%s>" addr)

    method rcpt addr = self#smtp_cmd (Printf.sprintf "RCPT TO:<%s>" addr)
    
    method data email_string =
      self#smtp_cmd "DATA";
      email_string |>
        Str.global_replace new_line_regexp "\r\n" |>
          Str.split crlf_regexp |>
            List.iter (fun s ->
              self#output_string (if String.length s > 0 && s.[0] = '.' then
                                    ("." ^ s ^ "\r\n")
                                  else s^"\r\n"));
      self#smtp_cmd "."

    method quit =
      self#smtp_cmd "QUIT"

    method disconnect =
      match self#get_channel with
        | Unix_socket s -> Unix.close s
        | SSL_socket s -> Ssl.shutdown s

  end

let sendmail conf_e subject body ?(attachment) () =

  let e_smtp = conf_e.e_smtp in

  if !ssl_already_init = false then
    if e_smtp.sm_ssl then begin
      Ssl.init();
      ssl_already_init := true
    end
  ;

  let email_as_string = create_message conf_e subject body attachment in

  let client = new smtp_client e_smtp.sm_host e_smtp.sm_port in
    try
      client#connect;
      client#ehlo;
      if e_smtp.sm_ssl then
	begin
	  client#starttls;
	  client#ehlo;
	  match e_smtp.sm_credentials with
	    | None -> ()
	    | Some cred ->
	      client#login
		cred.cred_username
		cred.cred_passwd
	end;
      client#mail conf_e.e_sender_address;
      List.iter client#rcpt conf_e.e_recipients;
      client#data email_as_string;
      client#quit;
      client#disconnect
    with
      | SMTP_error (code, _) ->
	(Config.cfg)#set_email_disabled;
	let err = Printf.sprintf "SMTP error code %d. Sending of emails is disabled" code in
	Log.log (err, Error)
      | _ ->
	(Config.cfg)#set_email_disabled;
	Log.log ("An error occured while connecting to the smtp server. Sending of emails is disabled", Error)
