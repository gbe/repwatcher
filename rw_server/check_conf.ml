open Types_conf ;;
open Types ;;
open Unix ;;

(* Perform the following tests and failwith in case of error :
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

   - if log_directory can be read and written
 *)
let check conf conf_path =


  (* Test if we can successfully connect to the SGBD without a dbname,
   * because at this point, the DB could not exist.
   *
   * if we can't, then we exit right away
   * This prevents the program to crash long after starting running it
   * at the very moment a SQL query is needed.
   * With this, we know from the start if (at least this part) it goes right or wrong.
   * There is no need to add a try/with here because it's handled in Mysqldb.ml
   *)  
  begin
    match Mysqldb.connect_without_db () with
      | None -> failwith "Could not connect, read the log"
      | Some cid ->
	Mysqldb.disconnect cid
  end ;



  begin
    match conf.c_mysql.dbname with
      | None -> assert false
      | Some dbname ->

	(* if Mysqldb.create_db goes wrong, the program exits *)
	Mysqldb.create_db dbname ;

	(* if Mysqldb.create_table_accesses goes wrong, the program exits *)
	Mysqldb.create_table_accesses () ;

  end ;



 Log.log ("helloOOooo", Error);



  (* Check if the identity which should be taken
   * by the main process exists
   * (only if the current identity is root) *)
  begin
    match conf.c_process_identity with
    | None -> ()
    | Some new_main_identity ->
	if Unix.geteuid () = 0 && Unix.getegid () = 0 then begin
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
	end
  end;

  (* print and log if others have read permission on file *)
  let check_rights file =
    let rights = Printf.sprintf "%o" ((Unix.stat file).st_perm) in
    if int_of_string (Str.last_chars rights 1) != 0 then
      Log.log ("Warning: "^file^" is accessible by the group 'other'", Error)
  in
  check_rights conf_path ;


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

  (* If the server is enabled *)
  if conf.c_notify.n_remotely then
    begin
      match conf.c_server with
      | None -> assert false
      | Some server ->
	  begin
	    match server.s_certs with
	    | None -> assert false
	    | Some certs -> 
		(* checks the CA *)
		can_be_accessed certs.c_ca_path [F_OK ; R_OK];

		(* checks the cert *)
		can_be_accessed certs.c_serv_cert_path [F_OK ; R_OK];

		(* checks the key *)
		can_be_accessed certs.c_serv_key_path [F_OK ; R_OK];
		check_rights certs.c_serv_key_path
	  end;

	  (* Check if the directory to chroot exists *)
	  begin
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
	  end;

          (* Check if the identity which should be taken by
	   * the remote process exists (only if the current identity is root)
	   *)
	  begin
	    match server.s_process_identity with
	    | None -> ()
	    | Some new_remote_identity ->
		if Unix.geteuid() = 0 && Unix.getegid() = 0 then begin
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
		end;
	  end;
    end;


  (* is_directory raises "no such file or directory" if l_directory doesn't exist.
   * The exception isn't caught to let it exit the program *)
  if Sys.is_directory conf.c_log.l_directory then
    can_be_accessed conf.c_log.l_directory [R_OK ; W_OK]
  else
    failwith (conf.c_log.l_directory^" is not a directory")
;;
