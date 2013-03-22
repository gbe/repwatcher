open Unix
open Printf

open Types
open Types_conf

let usage = "usage: rw_server [-f Configuration file path]" ;;

(* Configuration file fullpath *)
let config_file = ref "repwatcher.conf" ;;

let clean_exit () =
  Unix.close Core.core#get_fd ;

  (* No need to handle SQL because each connection is closed immediately
   * However, we do need to set all the IN_PROGRESS accesses to zero.
   * This has been proved to be usefull for outside apps *)
  Mysqldb.mysql#sgbd_reset_in_progress
;;

let drop_identity checker new_identity =
  
  (* Drop privileges by changing the processus' identity
   * if its current id is root *)
  if Unix.geteuid() = 0 && Unix.getegid() = 0 then begin

    (* Should be performed before dropping root rights (if any) *)
    checker#process_identity ;

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
;;

(* Main function *)
let _ =  

  Printf.printf "\nRepwatcher  Copyright (C) 2007-2013  Grégory Bellier
This program comes with ABSOLUTELY NO WARRANTY.
This is free software under the MIT license.\n\n";
  Pervasives.flush Pervasives.stdout;

  Arg.parse
    [ 
      "-f", Arg.String (fun path_conf ->
	config_file := path_conf),
      ("\tConfiguration file path: /<some_path>/"^(!config_file));
    ]
    (fun _ -> print_endline usage ; exit 1 ) usage;  


  (* Need to be after Arg.parse, otherwise there are problems to
   * display usage and the -help and --help options *)
  at_exit clean_exit;

  Config.cfg#parse !config_file;
  let conf = Config.cfg#get in
  let checker = new Check_conf.config_checker conf in

  (* Check if a connection can be done with the SMTP server *)
  checker#check_smtp_server;


  (* Fork if remote notifications are activated *)
  let fd =
    match conf.c_notify.n_remotely with
    | true  ->

      (* Should be performed before dropping root rights (if any) *)
      checker#remote_process_identity;
      (* Should be performed before dropping root rights (if any) *)
      checker#chroot;

      (* Match left here willingly *)
      begin match conf.c_server with
	| None -> assert false
	| Some server ->
	  Log.log ("Start server for remote notifications", Normal_Extra) ;

	  (* Pipes initialization must be done before the fork *)
	  Pipe.father2child#create;
	  Pipe.child2father#create;

	  Unix.fork ();
      end
    | false -> -1
  in
  
  
  (* If the process has been forked *)
  match fd with
    | 0 ->
      if conf.c_notify.n_remotely then begin

	(* Start the server *)
	match conf.c_server with
	  | None -> assert false
	  | Some server ->
	    Ssl_server.run Pipe.father2child#get_toread server
      end

    | _ ->

      begin match conf.c_process_identity with
	| None -> ()
	| Some new_identity -> drop_identity checker new_identity
      end;

      begin
	match conf.c_mysql with
	  | None -> ()
	  | Some mysql ->

	    match mysql.dbname with
	      | None -> assert false
	      | Some dbname ->

		(* if Check_conf.sql_connection goes wrong, the program exits *)
		checker#sql_connection ;
	    
		(* if Mysqldb.create_db goes wrong, the program exits *)
		Mysqldb.mysql#create_db dbname ;

		(* if Mysqldb.create_table_accesses goes wrong, the program exits *)
		Mysqldb.mysql#create_table_accesses ;

		(* Set to zero every files marked as 'in progress' in the SGBD *)
		Mysqldb.mysql#sgbd_reset_in_progress ;
      end ;

      checker#rights !config_file ;
      
      (* If the server is enabled *)
      if conf.c_notify.n_remotely then
	checker#server_certs ;
     
      (* Watch the config *)
      Core.core#add_watch !config_file None true;
      
      if conf.c_notify.n_remotely then begin
	ignore (Thread.create Pipe_from_server_thread.wait_pipe_from_child_process ())
      end;


      (* Filter the directories given in the config file with the ignored ones *)
      let (dirs, children) =
	Dirs.filter_and_get_children
	  conf.c_watch.w_directories
	  conf.c_watch.w_ignore_directories
      in

      (* Watch the directories and their children *)
      List.iter (fun dir -> Core.core#add_watch dir None false) dirs;
      Core.core#add_watch_children children;

      ignore (Thread.create Offset_thread.loop_check ()) ;

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
	let event_l = 
	  try
	    ignore (Unix.select [ Core.core#get_fd ] [] [] (-1.));
	    Inotify.read Core.core#get_fd
	  with
	    (* triggered when ctrl+c is pressed *)
	    | Unix_error (_,"select",_) -> [] 
	    | Unix_error (e, "read", _) -> []
	in
	List.iter Events.what_to_do event_l
      done;
;;
