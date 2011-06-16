open Unix
open Printf

open Types
open Types_conf
open Core

let usage = "usage: rw_server [-f Configuration file path]" ;;

(* Configuration file fullpath *)
let config_file = ref "repwatcher.conf" ;;



(* Check if the config exists and then parse it *)
let load_config () =

  try
    (* Check if the file exists and if the process can read it *)
    Unix.access !config_file [F_OK ; R_OK];

    (* Parse the configuration file *)
    Config.parse !config_file

  with
    | Unix_error (error,_,file) ->
      let err =
	Printf.sprintf "%s: %s.\n\n%s\n" file (error_message error) usage
      in
      (* Do not log because we can't know
	 what the user decided to do about his log policy.
	 We could set it to log by default but I'd prefer to
	 not add extra code just for this. The program crashes and the user
	 is warned through the terminal that the configuration file
	 couldn't be accessed. It's enough, no need to log it.
      *)
      failwith err
;;



(* Watch the directories from the config file *) 
let watch_dirs directories ignore_directories =   
  
  let rem_slash l_directories =
    List.map (
    fun dir ->
      (* If the directory name ends with a '/' then it is deleted *)
      if String.get dir ((String.length dir) -1) = '/' then
	String.sub dir 0 ((String.length dir)-1)
      else dir
   ) l_directories
  in
  
  let directories = ref (rem_slash directories) in
  let ign_dirs_without_slash = rem_slash ignore_directories in
  
  let filter directories l_regexp =
    List.filter (
    fun dir ->
      try
	if Sys.is_directory dir then						
	  try
	    ignore (List.find (fun reg  -> Str.string_match reg dir 0) l_regexp);
	    false (* false = taken off the list *)
	  with Not_found -> true
	else
	  false
	    
      (* No such file or directory *)
      with Sys_error e ->
	let error = "Error: "^e in 
	prerr_endline error ;
	Log.log (error, Error) ;
	false
   ) directories
  in


  (*
   * Filter
   * - if it doesn't exist
   * - the subdirectories of an ignored one like /home/dest/Ftp/ignored/dir (set to be watched) and /home/dest/Ftp/ignored (set to be ignored)
   * /home/dest/Ftp/ignored/dir is taken off of the list
   *)
  let l_regexp_ign_dirs_slash_ending =
    List.map (
      fun ign_dir ->
	(* Str.quote escapes special chars which causes problems: $^.*+?[]
	 * Refer to Str manpage
	 *)
	Str.regexp ((Str.quote ign_dir)^"/")
    ) ign_dirs_without_slash
  in
  directories := filter !directories l_regexp_ign_dirs_slash_ending;


  (* This dollar is used for the regexp to keep *only* the folder "test - etc" (derivative names) in the following example :
     - /home/dest/test - etc
     - /home/dest/test      <---  this one is the ignored folder
   * Without it both directories would be ignored
   *
   * This regexp also takes off of the list an entry
   * which is the exact same one if it's set to be watched and ignored (what stupid people can do) :
     - to be watched: /home/dest/Ftp
     - to be ignored: /home/dest/Ftp
   *)
  let l_regexp_ign_dirs_dollar =
    List.map (
      fun ign_dir ->
	Str.regexp ((Str.quote ign_dir)^"$")
    ) ign_dirs_without_slash
  in
  directories := filter !directories l_regexp_ign_dirs_dollar;
  

  let children =
    List.fold_left (
    fun dirs2watch dir ->
      let children_of_a_branch =
	try
	  List.tl (Dirs.ls dir l_regexp_ign_dirs_dollar)
	with Failure _ -> []
      in
      children_of_a_branch@dirs2watch
   ) [] !directories
  in
  
  (* Watch the folders given in the config file... *)
  List.iter (fun dir -> Core.add_watch dir None false) !directories;

  (* ... then watch their subfolders *)
  Core.add_watch_children children
;;





let clean_exit () =
  Unix.close Core.fd ;

  (* No need to handle SQL because each connection is closed immediately
   * However, we do need to set all the IN_PROGRESS accesses to zero.
   * This has been proved to be usefull for outside apps *)
  Mysqldb.sgbd_reset_in_progress ()
;;










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
let check conf =


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
  check_rights !config_file ;


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





(* Main function *)
let _ =  

  Printf.printf "\nRepwatcher  Copyright (C) 2009-2011  Grégory Bellier
This program comes with ABSOLUTELY NO WARRANTY.
This is free software under the MIT license.\n\n";
  Pervasives.flush Pervasives.stdout;

  Arg.parse
    [ 
      "-f", Arg.String (fun path_conf ->
	config_file := path_conf),
      "\tConfiguration file path";
    ]
    (fun _ -> print_endline usage ; exit 1 ) usage;  


  (* Need to be after Arg.parse, otherwise there are problems to
   * display usage and the -help and --help options *)
  at_exit clean_exit;

  (* Load the configuration file *)
  let conf = load_config () in

  check conf;

  (* Set to zero every files marked as 'in progress' in the SGBD *)
  Mysqldb.sgbd_reset_in_progress ();

  (* Watch the config *)
  Core.add_watch !config_file None true;





  (* Fork if remote notifications are activated *)
  let fd =
    match conf.c_notify.n_remotely with
    | true  ->
	(* Match left here willingly *)
	begin match conf.c_server with
	| None -> assert false
	| Some server ->
	    Log.log ("Start server for remote notifications", Normal_Extra) ;
	    Unix.fork();
	end
    | false -> -1
  in
  
  
  (* If the process has been forked *)
  match fd with
  | 0 ->
      if conf.c_notify.n_remotely then begin
	match conf.c_server with
	| None -> assert false
	| Some server ->
	    Ssl_server.run Pipe.tor server
      end

  | _ ->
      if conf.c_notify.n_remotely then begin
	ignore (Thread.create Pipe_listening.wait_pipe_from_child_process ())
      end;


      begin
	match conf.c_process_identity with
	| None -> ()
	| Some new_identity ->
	    (* Drop privileges by changing the processus' identity
	     * if its current id is root *)
	    if Unix.geteuid() = 0 && Unix.getegid() = 0 then
	      begin
		try
		  (* Check in the file /etc/passwd
		   * if the user "new_identity" exists *)
		  let passwd_entry = Unix.getpwnam new_identity in
		  setgid passwd_entry.pw_gid;
		  setuid passwd_entry.pw_uid;

		with Not_found ->
		  (* This shouldn't be triggered here
		   * because the test has been already done
		   * in the function check() *)
		  let error =
		    "Fatal error. User "^new_identity^" doesn't exist. \
                     The process can't take this identity"
		  in
		  Log.log (error, Error);
		  failwith error
	      end
      end;


      (* watch the directories given in the config file *)
      watch_dirs conf.c_watch.w_directories conf.c_watch.w_ignore_directories;

      ignore (Thread.create Offset.loop_check ()) ;

      ignore (Report.Report.report ( Notify ( Local_notif "Repwatcher is watching youuu ! :)" ) ) );
      Log.log ("Repwatcher is watching youuu ! :)", Normal) ;


      (* **************************** *)
      (* For interruptions *)
      let loop = ref true in
	
      let handle_interrupt i =
	loop := false
      in
      ignore (Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_interrupt));
      ignore (Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt));
      (* **************************** *)

      while !loop do
	try
	  let _,_,_ = Unix.select [ Core.fd ] [] [] (-1.) in
	  let event_l = Inotify.read Core.fd in
	  List.iter (fun event -> Core.what_to_do event) event_l
	with Unix_error (_,_,_) -> () (* Unix.select triggers this error when ctrl+c is pressed *)
      done;
;;
