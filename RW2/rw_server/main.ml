(*
    Repwatcher
    Copyright (C) 2009-2011  Gregory Bellier

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)



open Unix
open Printf

open Types
open Types_conf
open Core


let config_file = "conf/repwatcher.conf"    (* Nom du fichier de configuration *)



(* Check if the config exists and then
 * - Parse it
 * - Watch on it
 *)
let load_and_watch_config () =
  
  if Sys.file_exists config_file then
    begin
      (* Parse the configuration file *)
      let conf = Config.parse config_file in
      (* Watch it *)
      Core.add_watch config_file None true;
      conf
    end
  else
    failwith "Config file doesn't exist"
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
   * - the subdirectories of an ignored one like /home/dest/ignored/dir (set to be watched) and /home/dest/Ftp/ignored (set to be ignored)
   * /home/dest/ignored/dir is taken off the list
   *)
  let ignore_directories_slash = List.map (fun s -> s^"/") (rem_slash ignore_directories) in
  let regexp_ignore_directories_slash = List.map Str.regexp ignore_directories_slash in

  directories := filter !directories regexp_ignore_directories_slash;

  (* This dollar is used for the regexp to keep *only* the folder "test - etc" (derivative names) in the following example :
     - /home/dest/test - etc
     - /home/dest/test      <---  this one is the ignored folder
   * Without it both directories would be ignored
   *
   * This regexp also takes off the list an entry which is the exact same one if it's set to be watched and ignored (what stupid people can do) :
     - to be watched: /home/dest/Ftp
     - to be ignored: /home/dest/Ftp
   *)
  let ignore_directories_dollar = List.map (fun s -> s^"$") (rem_slash ignore_directories) in
  let regexp_ignore_directories_dollar = List.map Str.regexp ignore_directories_dollar in

  directories := filter !directories regexp_ignore_directories_dollar;
  
  let children =
    List.fold_left (
    fun dirs2watch dir ->
      let children_of_a_branch =
	try
	  List.tl (Dirs.ls dir regexp_ignore_directories_dollar)
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


let sgbd_reset_in_progress () =
  (* Reset all IN_PROGRESS accesses in the SGBD *)
  let reset_accesses =
    "UPDATE downloads SET IN_PROGRESS = '0' WHERE IN_PROGRESS = '1'"
  in
  ignore (Mysqldb.query reset_accesses)
;;

let clean_exit () =
  Unix.close Core.fd ;

  (* No need to handle SQL because each connection is closed immediately
   * However, we do need to set all the IN_PROGRESS access to zero.
   * This has been proved usefull for outside apps *)
  sgbd_reset_in_progress ()
;;


let check conf =
  (* Test if we can successfully connect to the SGBD
   * if we can't, then we exit right away
   * This prevents the program to crash long after starting running it
   * at the very moment a SQL query is needed.
   * With this, we know from the start if (at least this part) it goes right or wrong.
   * There is no need to add a try/with here because it's handled in Mysqldb.ml
   *)  

  begin
    match Mysqldb.connect() with
    | None -> failwith "Could not connect, read the log"
    | Some cid ->
	match Mysqldb.disconnect cid with
	| true -> ()
	| false -> failwith "Could not disconnect, read the log"
  end;



  (* Check if the identity which should be taken by the main process exists (only if the current identity is root) *) 
  begin
    match conf.c_main_proc_id_fallback with
    | None -> ()
    | Some new_main_identity ->
	if Unix.geteuid() = 0 && Unix.getegid() = 0 then
	  begin
	    try
	      (* Check in the file /etc/passwd if the user "new_main_identity" exists *)
	      ignore (Unix.getpwnam new_main_identity);
	    with Not_found ->
	      let error =
		"Fatal error. User "^new_main_identity^" doesn't exist. The process can't take this identity" in
	      Log.log (error, Error);
	      failwith error
	  end
  end;


  (* Check if the directory to chroot the network process exists *)
  if conf.c_notify.n_remotely.r_activate then
    begin
      match conf.c_notify.n_remotely.r_chroot with
      | None -> ()
      | Some dir ->
	  try
	    match Sys.is_directory dir with
	    | true -> ()
	    | false ->
		let error = "Can't chroot in "^dir^", it's not a directory" in
		Log.log (error, Error);
		failwith error
	  with Sys_error err ->
	    let error = "Can't chroot in "^dir^", it's not a directory. "^err in
	    Log.log (error, Error);
	    failwith error
    end
;;




(* Fonction main *)
let _ =
  at_exit clean_exit;

  Printf.printf "\nRepwatcher  Copyright (C) 2009-2011  Gregory Bellier
This program comes with ABSOLUTELY NO WARRANTY; for details read COPYING file.
This is free software, and you are welcome to redistribute it
under certain conditions; for details read COPYING file\n\n";
  Pervasives.flush Pervasives.stdout;



  (* Load and watch the configuration file *)
  let conf = load_and_watch_config () in
  
  check conf;

  sgbd_reset_in_progress ();

	



 
(* Fork if remote notifications are activated *)
  let fd =
    match conf.c_notify.n_remotely.r_activate with
    | true  ->
	
        (* Check if the identity exists which should be taken by the remote process (only if the current identity is root) *) 
	begin
	  match conf.c_notify.n_remotely.r_process_identity with
	  | None -> ()
	  | Some new_remote_identity ->
	      if Unix.geteuid() = 0 && Unix.getegid() = 0 then
		begin
		  try
		    (* Check in the file /etc/passwd if the user "new_remote_identity" exists *)
		    ignore (Unix.getpwnam new_remote_identity);
		  with Not_found ->
		    let error = "Fatal error. User "^new_remote_identity^" doesn't exist. The network process can't take this identity" in
		    Log.log (error, Error);
		    failwith error
		end
	end;
	
	Log.log ("Start server for remote notifications", Normal_Extra) ;
	Unix.fork()
    | false -> -1
  in
  
  
(* If the process has been forked *)   
  match fd with
  | 0 ->
      if conf.c_notify.n_remotely.r_activate then
	Ssl_server.run Pipe.tor conf.c_notify.n_remotely
  | _ ->
      begin
	if conf.c_notify.n_remotely.r_activate then begin
	  ignore (Thread.create Pipe_listening.wait_pipe_from_child_process ())
	end;
	
	
	begin
	  match conf.c_main_proc_id_fallback with
	  | None -> ()
	  | Some new_identity ->
	      (* Drop privileges by changing the processus' identity if its current id is root *)
	      if Unix.geteuid() = 0 && Unix.getegid() = 0 then
		begin
		  try
		    (* Check in the file /etc/passwd if the user "new_identity" exists *)
		    let passwd_entry = Unix.getpwnam new_identity in
		    setgid passwd_entry.pw_gid;
		    setuid passwd_entry.pw_uid;
		  with Not_found ->
		    let error = "Fatal error. User "^new_identity^" doesn't exist. The process can't take this identity" in
		    Log.log (error, Error);
		    failwith error
		end
	end;
	
	
	(* watch the directories given in the config file *)
	watch_dirs conf.c_watch.w_directories conf.c_watch.w_ignore_directories;
	
	Report.Report.report ( Notify ( Local_notif "Repwatcher is watching youuu ! :)" ) ) ;
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

      end
;;
