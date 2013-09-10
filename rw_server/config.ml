open Lexing
open Format
open Types
open Types_conf
open Unix

exception Config_error ;;
exception Email_not_configured ;;
exception Process_identity_not_configured ;;
exception SQL_not_configured ;;

let usage = "usage: rw_server [-f Configuration file path]" ;;

class config =
object(self)

  val mutable conf = None

(* localize the error and give the line and column *)
  method private _localization (pos,e) config_file =
    let l = pos.pos_lnum in
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    let lc = e.pos_cnum - pos.pos_bol + 1 in
    eprintf "File \"%s\", line %d, character %d-%d:\n" config_file l c lc

  method get =
    match conf with
      | None ->
      (* This exception should never be raised but there is a case
       * when it can be raised. When there is a problem parsing the
       * configuration file, it raises a Parse_error. The program
       * must exit right away but clean_exit() is then executed from
       * main.ml. The function tries to clean up the RDBMS and tries to
       * connect using the info from the configuration which doesn't
       * exist due to the Parse_error. *)
	raise Config_error

      | Some c -> c

  method parse cfg_file =
    begin try
    (* Check if the file exists and if the process can read it *)
      Unix.access cfg_file [F_OK ; R_OK];
    with
      | Unix_error (error,_,file) ->
	let err =
	  Printf.sprintf "%s: %s.\n\n%s\n" file (error_message error) usage
	in
      (* Do not log because we can't know what
	 the user decided to do about his log policy
	 and where he chose to log
      *)
	failwith err
    end;

    let c = open_in cfg_file in
    let lb = Lexing.from_channel c in

    begin try
      conf <- Some (Parser.configuration Lexer.nexttoken lb)
    with
      | Lexer.Lexing_error s -> 
	self#_localization (lexeme_start_p lb, lexeme_end_p lb) cfg_file;
	eprintf "lexical error in the configuration file %s: %s\n@." cfg_file s;
	exit 1
      | Parsing.Parse_error ->
	self#_localization (lexeme_start_p lb, lexeme_end_p lb) cfg_file;
	eprintf "syntax error in the configuration file %s\n@." cfg_file;
	exit 1
    end;

    close_in c;


  method get_email =
    let c =
      try
	(self#get).c_email 
      with Config_error ->
	let err = "Cannot retrieve email configuration since the config file has not been parsed" in
	Log.log (err, Error);
	exit 1
    in
    match c with
      | None -> raise Email_not_configured
      | Some email_conf -> email_conf

  method is_email_activated =
   try
     ignore (self#get_email);
     true
   with Email_not_configured -> false


  method set_email_disabled =
    conf <- Some { self#get with c_email = None }

  method get_sql =
      let c =
	try
	  (self#get).c_sql
	with Config_error ->
	  let err = "Cannot retrieve SQL configuration since the config file has not been parsed" in
	 Log.log (err, Error);
	  exit 1
      in
      match c with
	| None -> raise SQL_not_configured
	| Some sql_conf -> sql_conf


  method is_sql_activated =
       try
     ignore (self#get_sql);
     true
   with SQL_not_configured -> false

  method get_sql_rdbms =
    try
      (self#get_sql).sql_rdbms
    with SQL_not_configured ->
      Log.log ("Cannot get the RDBMS since SQL is not configured", Error);
      exit 1


  method get_process_identity =
    let c =
      try
	(self#get).c_process_identity
      with Config_error ->
	let err =
	  "Cannot retrieve process identity configuration since the config file has not been parsed"
	in
	Log.log (err, Error);
	exit 1
    in
    match c with
      | None -> raise Process_identity_not_configured
      | Some main_process_id -> main_process_id

  method get_log_verbosity =
    try
      (self#get).c_log
    with Config_error ->
      let err =
	"Cannot retrieve the log verbosity since the config file has not been parsed"
      in
      Log.log (err, Error);
      exit 1





  (* *** Config checker *** *)


  (* The following tests failwith in case of error :
     - main process identity
     - rights on the configuration file

     if the server is enabled :
     - exist and can be read: CA, cert
     - exist, can be read and rights: key
     - chroot folder
     - server process identity
     
     - if the smtp server can be reached
  *)




(* Check if the identity which should be taken
 * by the main/remote process exists
 * (only if the effective current identity is root)
 *)
  method private _check_identity identity =
    if Unix.geteuid () = 0 && Unix.getegid () = 0 then
      try
	(* Check in /etc/passwd if the user "identity" exists *)
	ignore (Unix.getpwnam identity);
      with Not_found ->
	let error =
	  "Fatal error. User "^identity^" does not exist. \
            The process cannot take this identity"
	in
	Log.log (error, Error);
	failwith error

  method check_process_identity =
    try
    let proc_id = self#get_process_identity in
    self#_check_identity proc_id
    with Process_identity_not_configured ->
      Log.log ("Cannot check the process identity since it is disabled", Normal_Extra)


(* Check if the identity which should be taken by
 * the remote process exists (only if the current identity is root)
 *)
  method check_remote_process_identity =
    match (self#get).c_server with
      | None -> assert false
      | Some server ->
	match server.s_process_identity with
	  | None -> ()
	  | Some new_remote_identity ->
	    self#_check_identity new_remote_identity


  (* print and log if the 'others' have read permission on file *)
  method rights file =
    let rights = Printf.sprintf "%o" ((Unix.stat file).st_perm) in
    if int_of_string (Str.last_chars rights 1) != 0 then
      let err =
	"Warning: "^file^" is readable by the group 'other'. \
         You should changed this to prevent passwords/private key leakage"
      in 
      Log.log (err, Error)


  method server_certs =
    match (self#get).c_server with
      | None -> assert false
      | Some server ->
	match server.s_certs with
	  | None -> assert false
	  | Some certs -> 

	    (* Does the file exist and can it be read ? *)
	    let can_be_accessed file_dir rights =
	      try
		(* Checks if the file exists and if the process can read it *)
		Unix.access file_dir rights ;
		
	      with Unix_error (error,_,file_dir') ->
		let err = Printf.sprintf "%s: %s" file_dir' (error_message error) in
		Log.log (err, Error) ;
		failwith err
	    in

	    (* checks the CA *)
	    can_be_accessed certs.c_ca_path [F_OK ; R_OK];

	    (* checks the cert *)
	    can_be_accessed certs.c_serv_cert_path [F_OK ; R_OK];

	    (* checks the key *)
	    can_be_accessed certs.c_serv_key_path [F_OK ; R_OK];
	    self#rights certs.c_serv_key_path


  (* Check if the directory to chroot exists *)
  method chroot =
    match (self#get).c_server with
      | None -> assert false
      | Some server ->
	match server.s_chroot with
	  | None -> ()
	  | Some dir ->
	    try
	      match Sys.is_directory dir with
		| true -> ()
		| false ->
		  let error =
		    "Cannot chroot in "^dir^", it is not a directory"
		  in
		  Log.log (error, Error);
		  failwith error
	    with Sys_error err ->
	      let error =
		"Cannot chroot in "^dir^", it is not a directory or it does not exist. "^err
	      in
	      Log.log (error, Error);
	      failwith error


  (* Check if a connection can be done with the SMTP server *)
  method check_smtp_server =
    try
      let e = self#get_email in
      let host = e.e_smtp.sm_host in
      let port = e.e_smtp.sm_port in
      try
	let resolve name =
	  begin try Unix.inet_addr_of_string name
	    with Failure _ ->
              let h = Unix.gethostbyname name in
              h.Unix.h_addr_list.(0)
	  end
	in
    
	let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	Unix.connect s (Unix.ADDR_INET((resolve host), port));
	Unix.close s
      with | _ ->
	(* sending of emails disabled *)
	self#set_email_disabled;

	let err = 
	  "Err: while checking if repwatcher could \
connect to "^host^", an error occured. Sending of emails is disabled"
	in 
	Log.log (err, Error)
    with
      | Email_not_configured ->
	Log.log ("Do not check the SMTP server since emailing is disabled", Normal_Extra)


end ;;
		  
let cfg = new config ;;
