open Types_conf ;;
open Types ;;
open Unix ;;



(* The following tests failwith in case of error :
   - SGBD connection
   - Add Database if it doesn't exist
   - Add Table
   - main process identity
   - rights on the configuration file

   if the server is enabled :
   - exist and can be read: CA, cert
   - exist, can be read and rights: key
   - chroot folder
   - server identity

   - if the smtp server can be reached
 *)

class config_checker conf =
object(self)

  (* Does the file exist and can it be read ? *)
  method private can_be_accessed file_dir rights =
    try
      (* Checks if the file exists and if the process can read it *)
      Unix.access file_dir rights ;
      
    with Unix_error (error,_,file_dir') ->
      let err = Printf.sprintf "%s: %s" file_dir' (error_message error) in
      Log.log (err, Error) ;
      failwith err


(* Test if we can successfully connect to the SGBD without a dbname,
 * because at this point, the DB could not exist.
 *
 * if we can't, then we exit right away
 * This prevents the program to crash long after starting running it
 * at the very moment a SQL query is needed.
 * With this, we know from the start if (at least this part) it goes right or wrong.
 * There is no need to add a try/with here because it's handled in Mysqldb.ml
 *)  
  method sql_connection =
    match Mysqldb.connect_without_db () with
      | None -> failwith "Could not connect to MySQL server, read the log"
      | Some cid ->
	Mysqldb.disconnect cid



(* Check if the identity which should be taken
 * by the main process exists
 * (only if the current identity is root)
 *)
  method process_identity =  
    match conf.c_process_identity with
      | None -> ()
      | Some new_main_identity ->
	if Unix.geteuid () = 0 && Unix.getegid () = 0 then
	  try
	  (* Check in /etc/passwd if the user "new_main_identity" exists *)
	    ignore (Unix.getpwnam new_main_identity);
	  with Not_found ->
	    let error =
	      "Fatal error. User "^new_main_identity^" doesn't exist. \
            The process can't take this identity"
	    in
	    Log.log (error, Error);
	    failwith error


(* print and log if others have read permission on file *)
  method rights file =
    let rights = Printf.sprintf "%o" ((Unix.stat file).st_perm) in
    if int_of_string (Str.last_chars rights 1) != 0 then
      Log.log ("Warning: "^file^" is accessible by the group 'other'", Error)


  method server_certs =
    match conf.c_server with
      | None -> assert false
      | Some server ->
	match server.s_certs with
	  | None -> assert false
	  | Some certs -> 
	    (* checks the CA *)
	    self#can_be_accessed certs.c_ca_path [F_OK ; R_OK];

	    (* checks the cert *)
	    self#can_be_accessed certs.c_serv_cert_path [F_OK ; R_OK];

	    (* checks the key *)
	    self#can_be_accessed certs.c_serv_key_path [F_OK ; R_OK];
	    self#rights certs.c_serv_key_path


  (* Check if the directory to chroot exists *)
  method chroot =
    match conf.c_server with
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
		    "Can't chroot in "^dir^", it's not a directory"
		  in
		  Log.log (error, Error);
		  failwith error
	    with Sys_error err ->
	      let error =
		"Can't chroot in "^dir^", it's not a directory. "^err
	      in
	      Log.log (error, Error);
	      failwith error


(* Check if the identity which should be taken by
 * the remote process exists (only if the current identity is root)
 *)
  method remote_process_identity =
    match conf.c_server with
      | None -> assert false
      | Some server ->
	match server.s_process_identity with
	  | None -> ()
	  | Some new_remote_identity ->
	    if Unix.geteuid() = 0 && Unix.getegid() = 0 then
	      try
		(*
		 * Check in /etc/passwd
		 * if the user "new_remote_identity" exists
		 *)
		ignore (Unix.getpwnam new_remote_identity);
	      with Not_found ->
		let error =
		  "Fatal error. User "^new_remote_identity^" doesn't exist. \
                The network process can't take this identity"
		in
		Log.log (error, Error);
		failwith error


  (* Check if a connection can be done with the SMTP server *)
  method check_smtp_server =

    match conf.c_email with
      | None -> ()
      | Some e ->
	let host = e.e_smtp.sm_host in
	let port = e.e_smtp.sm_port in

	try
	  let resolve name =
	    try Unix.inet_addr_of_string name
	    with Failure _ ->
              let h = Unix.gethostbyname name in
              h.Unix.h_addr_list.(0)
	  in
    
	  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	  Unix.connect s (Unix.ADDR_INET((resolve host), port));
	  Unix.close s
	with _ ->
	  (* sending of emails disabled *)
	  Config.cfg#set_email_disabled;

	  let err = 
	    "Err: while checking if repwatcher could \
connect to "^host^", an error occured. Sending of emails is disabled"
	  in 
	  Log.log (err, Error)

end;;
