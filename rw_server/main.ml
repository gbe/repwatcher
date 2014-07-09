open Unix
open Printf
open Types
open Types_conf
open Config
open Files_progress

let usage = "usage: rw_server [-f Configuration file path]" ;;

(* Configuration file fullpath *)
let config_file = ref "repwatcher.conf" ;;
let is_sql_activated_and_working = ref false ;;

let clean_exit () =
  Unix.close InotifyCaller.core#get_fd ;

   (* we need to set all the IN_PROGRESS accesses to zero.
   * This has been proven to be useful for outside apps *)
  if !is_sql_activated_and_working then
    begin
      let sql = new Sqldb.sqldb in
      sql#reset_in_progress;

      (* Cleanup every ongoing prepared stmts
       * for every files in progress and disconnect from RDBMS *)
      Hashtbl.iter (fun (_) in_progress  ->
	match in_progress.ip_sql_connection with
	  | None -> ()
	  | Some sqlobj ->
	    sqlobj#disconnect ()

      ) Files_progress.ht;
    end
;;


let change_main_process_identity () =

  (* Drop privileges by changing the processus' identity
   * if its current id is root *)
  if Unix.geteuid () = 0 && Unix.getegid () = 0 then begin

    (* Does the new identity exist ? Failwith in case it does not *)
    Config.cfg#check_process_identity ;

    try
      (* Check in the file /etc/passwd
       * if the user "new_identity" exists *)
      let passwd_entry = Unix.getpwnam Config.cfg#get_process_identity in
      initgroups Config.cfg#get_process_identity passwd_entry.pw_gid;
      setuid passwd_entry.pw_uid;

    with
      | Process_identity_not_configured ->
	let error =
	  "Fatal programming error. The process identity was not set \
           in the configuration file, the process cannot take the identity."
	in
	Log.log (error, Error);
	failwith error
      | Not_found ->
	let error =
	  "Fatal error. User "^Config.cfg#get_process_identity^" does not exist \
           in the configuration file. Repwatcher's main process cannot take this \
           identity and will therefore exit"
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
    | true -> sql#disconnect ()
;;


(* Main function *)
let _ =

  Printf.printf "\nRepwatcher  Copyright (C) 2007-2014  Grégory Bellier
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
  (* Prior to the config file parsing, the "log" assumed it was on by default.
   * Now that the config file has been parsed,
   * it must be informed of the real value: en/disabled, debug, ... *)
  let conf = Config.cfg#get in

  (* This must be the first thing done after having parsed the config file *)
  Log.sysl#set_config Config.cfg#get_log_verbosity ;

  (* Check if the config file permission has read rights for the group others
   * If it does, it is a user mistake *)
  Config.cfg#rights !config_file ;

  (* Fork if remote notifications are activated *)
  let fd =
    match conf.c_notify.n_remotely with
    | true  ->

      (* Checks if the new remote process identity and chroot exist
       * The actual operations are not done yet as those checks must be done before
       * forking with still the root rights *)
      Config.cfg#check_remote_process_identity;
      Config.cfg#chroot;

      (* Match left here willingly *)
      begin match conf.c_server with
	| None -> assert false
	| Some server ->
	  Log.log ("Creates the pipes and forks the process", Normal_Extra) ;

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
      (* Stuff to do in the child process with lesser privileges *)
      if conf.c_notify.n_remotely then begin

	(* Start the remote notification server *)
	match conf.c_server with
	  | None -> assert false
	  | Some server ->
	    Log.log ("Start remote notifications server", Normal_Extra) ;
	    Ssl_server.run Pipe.father2child#get_toread server
      end

    | _ ->
      (* Stuff to do in the father process *)

      (* First thing to do is to drop privileges *)
      if Config.cfg#is_process_identity_configured then
	change_main_process_identity ();

      (* Not only used to update the offsets in SQL,
       * it is also used to force the closing event *)
      ignore (Thread.create Offset_thread.loop_check ());

      if Config.cfg#is_sql_activated then begin
	let c_sql = Config.cfg#get_sql in

	(* if a connection to the SQL backend is not possible
	 * then the program exits *)
	check_sql_connection ();

	(* since the sql_connection tested above works
	 * then sql is activated for the at_exit call *)
	is_sql_activated_and_working := true;

	let sql = new Sqldb.sqldb in

	(* if Sqldb.create_db goes wrong, the program exits *)
	sql#create_db c_sql.sql_dbname ;

	(* if Sqldb.create_table_accesses goes wrong, the program exits *)
	sql#create_table_accesses ;

	(* Set to zero every files marked as 'in progress' in the RDBMS *)
	sql#reset_in_progress ;
      end ;

      (* Check if a connection can be done with the SMTP server *)
      Config.cfg#check_smtp_server;

      (* Start the pipes if the remote notification server is enabled *)
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
      List.iter (fun dir ->
	InotifyCaller.core#add_watch dir ~wd_father_opt:None ~is_config_file:false
      ) dirs;

      InotifyCaller.core#add_watch_children children;

      let notif_txt = "Repwatcher is watching for youuu ! :)" in
      print_endline notif_txt;
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
	    ignore (Unix.select [ InotifyCaller.core#get_fd ] [] [] (-1.));
	    Inotify.read InotifyCaller.core#get_fd
	  with
	    (* triggered when ctrl+c is pressed *)
	    | Unix_error (_,"select",_) -> []
	    | Unix_error (e, "read", _) -> []
	in
	List.iter Events.what_to_do event_l
      done;
;;
