open Types
open Types_conf
open Unix
open Dbus_call

let config_file = ref "rw_client.conf" ;;

let port = ref 9292 ;;
let host = ref "" ;;

(* will be changed if set on the CLI or when connected to the server *)
let nb_parent_folders = ref (-1) ;;


let parse_config file =
  try
    (* Check if the file exists and if the process can read it *)
    Unix.access file [F_OK ; R_OK];

    Config.parse file
    
  with Unix_error (error,_,file) ->
    let err = Printf.sprintf "%s: %s" file (error_message error) in
    failwith err
;;


let _ =

  
  let usage = "usage: rw_client host [-p port] [-f Configuration file path] [-n Folders_nb_for_notifications]" in
  
  Printf.printf "\nRepwatcher  Copyright (C) 2007-2013  Grégory Bellier
This program comes with ABSOLUTELY NO WARRANTY.
This is free software under the MIT license.\n\n";
  Pervasives.flush Pervasives.stdout;

  Arg.parse
    [
     "-p", Arg.Int (fun i -> port := i), "\tPort";

     "-f", Arg.String (fun path_conf -> config_file := path_conf), ("\tConfiguration file path: /<some_path>/"^(!config_file));

     "-n", Arg.Int (fun n ->
       if n < 0 then
	 raise (Arg.Bad ("wrong argument `"^(string_of_int n)^"'; option `-n' expects an unsigned integer"))
       else
	 nb_parent_folders := n
     ), "\tFolders_Number_For_Notifications";
    ]
    (fun s -> host := s) usage;
  
  if !host = "" then (Printf.printf "%s\n\n" usage; exit 1);

  (* print and log if others have read permission on file *)
  let check_rights file =
    let rights = Printf.sprintf "%o" ((Unix.stat file).st_perm) in
    if int_of_string (Str.last_chars rights 1) != 0 then
      begin
	let warning = "Warning: "^file^" is accessible by the group 'other'" in
	(* Log.log (warning, Error) *)
	prerr_endline warning
      end
  in
  
  (* Does the file exist and can it be read ? *)
  let exists_and_can_be_read file =
    try
      (* Checks if the file exists and if the process can read it *)
      Unix.access file [F_OK ; R_OK];

    with Unix_error (error,_,file') ->
      let err = Printf.sprintf "%s: %s" file' (error_message error) in
      (* Log.log (err, Error) ; *)
      failwith err
  in

  let conf = parse_config !config_file in
  let certs = conf.c_certs in

  (* check on CA *)
  exists_and_can_be_read certs.c_ca_path;
  
  (* check on cert *)
  exists_and_can_be_read certs.c_client_cert_path;

  (* checks on the key *)
  exists_and_can_be_read certs.c_client_key_path;
  check_rights certs.c_client_key_path ;
 

  Ssl_threads.init ();
  Ssl.init ();

  let he =
    (
     try
       gethostbyname !host
     with
     | Not_found -> failwith "Host not found"
    )
  in
  
  let sockaddr = ADDR_INET(he.h_addr_list.(0), !port) in
  let loop = ref true in
  let bufsize = 1024 in
  let buf = String.create bufsize in
  
  let s_ssl =
    let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in

    begin
      match certs.c_client_key_pwd with
	| None -> ()
	| Some pwd ->
	  Ssl.set_password_callback ctx (fun _ -> pwd)
    end;

    begin
      try
	Ssl.use_certificate ctx certs.c_client_cert_path certs.c_client_key_path
      with Ssl.Private_key_error -> failwith "Error with the private key, please check the password"
    end;
    
    Ssl.set_verify ctx [Ssl.Verify_peer] (Some Ssl.client_verify_callback);
   
    begin try
      Ssl.load_verify_locations ctx certs.c_ca_path (Filename.dirname certs.c_ca_path)
    with Invalid_argument e -> failwith ("Error_load_verify_locations: "^e)
    end;
    
  (*
   * Extracted from the SSL_CTX_set_verify man page
   * The depth count is "level 0:peer
   * certificate", "level 1: CA certificate", "level 2: higher level CA certificate", and so on.
   * Setting the maximum depth to 2 allows the levels 0, 1,
   * and 2. The default depth limit is 9, allowing for the peer certificate and additional 9 CA certificates.
   *)
    Ssl.set_verify_depth ctx 2;

    Ssl.open_connection_with_context ctx sockaddr
  in

  (* Check the result of the verification of the X509 certificate presented by
   * the peer, if any. Raises a [verify_error] on failure. *)
  begin
    try
      Ssl.verify s_ssl;
    with Ssl.Verify_error _ ->
      failwith (Ssl.get_error_string ())
  end;

  let handle_interrupt i =       
    begin try
      Ssl.output_string s_ssl "rw_client_exit"
    with Ssl.Write_error _ -> failwith "Failed to send the closing msg to server"
    end;
    Ssl.flush s_ssl;
    Ssl.shutdown_connection s_ssl;
    exit 0
  in
      
  ignore (Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_interrupt));
  ignore (Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt));

  let rec n_last_elements l =

    match l with
    | [] -> "./"
    | [t] -> t^"/"
    | t::q ->
	if List.length q < !nb_parent_folders then
	  t^"/"^(n_last_elements q)
	else
	  n_last_elements q
  in
  
  let regexp_slash = Str.regexp "/" in
  let regexp_space = Str.regexp "[' ']" in
  let regexp_ddot = Str.regexp "[':']" in

  begin try
    while !loop do
      let data_recv = Ssl.read s_ssl buf 0 bufsize in
      
      if data_recv > 0 then
	let data = String.sub buf 0 data_recv in
	let com = (Marshal.from_string data 0 : com_server2clients) in
	
	match com with
	| RW_server_exited -> loop := false
	| RW_server_con_ok nb_parent_folders_from_server -> 
	    begin
	      match !nb_parent_folders with
		| -1 -> (* Otherwise, we take the server's choice *)
		  nb_parent_folders :=
		    begin match nb_parent_folders_from_server with
		      | None -> 0
		      | Some nb -> nb
		    end
		| _ -> () (* It has been set by the client on the command line. *)
	      
	    end;
	    begin try
	      dbus "nobody" "Repwatcher" ("Successfully connected to "^(!host))
	    with _ -> prerr_endline "An error occured with dBus"
	    end
	      
	| Notification notif ->
	    begin
	      match notif with
	      (* Local_notifs can only be done server-side *)
	      | Local_notif _ -> assert false

	      | New_notif (file, filestate) -> 
		  
		  let msg_state =
		    match filestate with
		    | File_Created -> "has created"
		    | File_Opened -> "has opened"
		    | File_Closed -> "closed"
		  in
		  
		  Printf.printf "New notification received: '%s' %s '%s'\n" file.f2_username msg_state file.f2_name;
		  Pervasives.flush Pervasives.stdout;

		  let l_folders = Str.split regexp_slash file.f2_path in
		  let dbus_notif =
		    match !nb_parent_folders with
		      | 0 -> "<b>"^file.f2_username^"</b> "^msg_state^"\n"^file.f2_name
		      | _ -> "<b>"^file.f2_username^"</b> "^msg_state^"\n"^(n_last_elements l_folders)^file.f2_name
		  in

		  begin try
		    dbus "nobody" "Repwatcher" dbus_notif
		  with _ -> prerr_endline "An error occured with dBus"
		  end


	      | Old_notif dls_l ->

		  List.iter (
		  fun (file, date) ->
		    Printf.printf "Old notification received: At %s, '%s' opened '%s'\n" date file.f2_username file.f2_name;
		    Pervasives.flush Pervasives.stdout;
		    
		    let h_m_s = List.hd (List.tl (Str.split regexp_space date)) in
		    let h_m = 
		      let hms_l = Str.split regexp_ddot h_m_s in
		      (List.nth hms_l 0)^":"^(List.nth hms_l 1)
		    in

		    let l_folders = Str.split regexp_slash file.f2_path in
		    let dbus_notif =
		      match !nb_parent_folders with
			| 0 -> "<b>"^file.f2_username^"</b> opened\n"^file.f2_name
			| _ -> "<b>"^file.f2_username^"</b> opened\n"^(n_last_elements l_folders)^file.f2_name
		    in

		    begin try
 		      dbus "nobody" ("Repwatcher @ "^h_m) dbus_notif
		    with _ -> prerr_endline "An error occured with dBus"
		    end

		 ) dls_l
	    end		
    done
  with Ssl.Read_error _ -> ()
  end;
  
  Ssl.shutdown s_ssl;

  begin try
    dbus "nobody" "Repwatcher" "Server is down. Closing the client...";
  with _ -> prerr_endline "An error occured with dBus"
  end;

  exit 0	   
;;
