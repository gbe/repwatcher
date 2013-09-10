open Unix
open Printf

open Types
open Types_conf

let usage = "usage: rw_server [-f Configuration file path]" ;;

(* Configuration file fullpath *)
let config_file = ref "repwatcher.conf" ;;
let is_sql_activated = ref false ;;

let clean_exit () =
  Unix.close Core.core#get_fd ;

  (* No need to handle SQL because each connection is closed immediately
   * However, we do need to set all the IN_PROGRESS accesses to zero.
   * This has been proven to be useful for outside apps *)
  if !is_sql_activated then
    let sql = new Sqldb.sqldb in
    sql#reset_in_progress
;;


let drop_identity new_identity =
  
  (* Drop privileges by changing the processus' identity
   * if its current id is root *)
  if Unix.geteuid () = 0 && Unix.getegid () = 0 then begin

    (* Should be performed before dropping root rights (if any) *)
    Config.cfg#process_identity ;

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


let check_sql_connection () =
  let sql = new Sqldb.sqldb in
  sql#connect_without_db ;

  match sql#is_connected with
    | false -> failwith "Could not connect to SQL server, read the log"
    | true ->	sql#disconnect ()
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

  (* at_exit needs to be after Arg.parse, otherwise there are problems to
   * display usage and the -help and --help options *)
  at_exit clean_exit;

  Config.cfg#parse !config_file;
  (* Prior to the config file parsing, the "log" assumed it was default.
   * Now that the config file has been parsed, it must be informed of the real value *)
  let conf = Config.cfg#get in
  Log.sysl#set_config Config.cfg#get_log_verbosity ;

  (* Check if a connection can be done with the SMTP server *)
  Config.cfg#check_smtp_server;


  (* Fork if remote notifications are activated *)
  let fd =
    match conf.c_notify.n_remotely with
    | true  ->

      (* Should be performed before dropping root rights (if any) *)
      Config.cfg#remote_process_identity;
      (* Should be performed before dropping root rights (if any) *)
      Config.cfg#chroot;

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
	| Some new_identity -> drop_identity new_identity
      end;

      begin
	match conf.c_sql with
	  | None -> ()
	  | Some sqlparam' ->

	    (* if a connection to the SQL backend is not possible
	     * then the program exits *)
	    check_sql_connection ();
		
	    (* since the sql_connection tested above works
	     * then sql is activated for the at_exit call *)
	    is_sql_activated := true;

	    let sql = new Sqldb.sqldb in
		
	    (* if Sqldb.create_db goes wrong, the program exits *)
	    sql#create_db sqlparam'.sql_dbname ;

	    (* if Sqldb.create_table_accesses goes wrong, the program exits *)
	    sql#create_table_accesses ;

	    (* Set to zero every files marked as 'in progress' in the RDBMS *)
	    sql#reset_in_progress ;
      end ;

      Config.cfg#rights !config_file ;
      
      (* If the server is enabled *)
      if conf.c_notify.n_remotely then begin
	Config.cfg#server_certs ;
	ignore (Thread.create Pipe_from_server_thread.wait_pipe_from_child_process ())
      end; 
     
      (* Watch the config *)
      (* Core.core#add_watch !config_file ~wd_father_opt:None ~is_config_file:true; *)
      
      (* Filter the directories given in the config file with the ignored ones *)
      let (dirs, children) =
	Dirs.filter_and_get_children
	  conf.c_watch.w_directories
	  conf.c_watch.w_ignore_directories
      in

      (* Watch the directories and their children *)
      List.iter (fun dir -> Core.core#add_watch dir ~wd_father_opt:None ~is_config_file:false) dirs;
      Core.core#add_watch_children children;

      if Config.cfg#is_sql_activated then
	ignore (Thread.create Offset_thread.loop_check ());

      let notif_txt = "Repwatcher is watching youuu ! :)" in
      Report.report#notify (Local_notif notif_txt);
      Log.log (notif_txt, Normal) ;

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
