(*
    Repwatcher
    Copyright (C) 2009-2010  Gregory Bellier

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

open Ast
open Ast_conf
open Core
open Report


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
  
  let rem_slash l_directories = List.map (
    fun dir ->
      (* If the directory name ends with a '/' then it is deleted *)
      if String.get dir ((String.length dir) -1) = '/' then
	String.sub dir 0 ((String.length dir)-1)
      else dir
   ) l_directories
  in
  
  let directories = ref (rem_slash directories) in
  let ignore_directories_clean_name = rem_slash ignore_directories in

  let regexp_ignore_directories = List.map Str.regexp ignore_directories_clean_name in
  
  
  (* Filter if the directory in the config file
   * - exists
   * - is set to be watched BUT also ignored. Only someone stupid can do that
   *)
  directories := List.filter (
    fun dir ->
      try
	if Sys.is_directory dir then						
	  try
	    ignore (List.find (fun reg  -> Str.string_match reg dir 0) regexp_ignore_directories);
	    false
	  with Not_found -> true
	else
	  false
	    
      (* No such file or directory *)
      with Sys_error e ->
	let error = "Error: "^e in 
	  prerr_endline error ;
	  Report.report (Log (error, Error));
	  false
  ) !directories
    ;
  
  
  let children =
    List.fold_left (
    fun dirs2watch dir ->
      
      let dir_children = List.tl (Dirs.ls dir) in
      let dir_children_without_ignored_ones = 
	List.filter (
	fun dir_child ->
	  (* - if the exception is triggered, then it means that the directories ignored
	   * have nothing to do with this one
	   * 
	   * - This code is the same than above but without the test Sys.is_directory because I consider that the answer is true.
	   * I trust the result from "ls" therefore I skip a test for efficency.
	   *)
	  try
	    ignore (List.find (fun reg  -> Str.string_match reg dir_child 0) regexp_ignore_directories);
	    false
	  with Not_found -> true
       ) dir_children
      in
      dir_children_without_ignored_ones@dirs2watch
 					   
   ) [] !directories
  in
  
  (* Watch the folders given in the config file... *)
  List.iter (fun dir -> Core.add_watch dir None false) !directories;

  (* ... then watch their subfolders *)
  Core.add_watch_children children
;;
    




(* Fonction main *)
let _ =
  
  Printf.printf "\nRepwatcher  Copyright (C) 2009-2010  Gregory Bellier
This program comes with ABSOLUTELY NO WARRANTY; for details read COPYING file.
This is free software, and you are welcome to redistribute it
under certain conditions; for details read COPYING file\n\n";
  Pervasives.flush Pervasives.stdout;



  (* Load and watch the configuration file *)
  let conf = load_and_watch_config () in

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
	    with Not_found -> failwith ("Fatal error. User "^new_identity^" doesn't exist. The network process can't take this identity")
	  end;
  end;


  (* Test if we can successfully connect to the SGBD
   * if we don't, then we exit right now
   * This is added because the program crashed long after starting running it
   * at the very moment a SQL transaction was needed
   * With this, we know from the start if (at least this part) it goes right or wrong.
   * There is no need to add a try/with here because it's handled in Mysqldb.ml
   *)  
  begin
    match Mysqldb.connect() with
    | Some error -> 
	Report.report (Log (error, Error));
	failwith error
    | None       ->
	match Mysqldb.disconnect() with
	| Some error ->
	    Report.report (Log (error, Error));
	    failwith error
	| None -> ()
  end;
  
  (* watch the directories given in the config file *)
  watch_dirs conf.c_watch.w_directories conf.c_watch.w_ignore_directories;

  
  let fd = match conf.c_notify.n_remotely with
    | true  ->
	Report.report (Log ("Start server for remote notifications", Normal_Extra)) ;
	Unix.fork()
    | false -> -1
  in
      
     
    match fd with
      | 0 ->
	  if conf.c_notify.n_remotely then begin
	    Ssl_server.run Pipe.tor Pipe.tow2
	  end

      | _ ->
	  begin
	  if conf.c_notify.n_remotely then begin
	    ignore (Thread.create Pipe_listening.wait_pipe_from_child_process ())
	  end;
	    
	    Report.report ( Notify ( Info_notif "Repwatcher is watching youuu ! :)" )  ) ;
	    Report.report ( Log   ("Repwatcher is watching youuu ! :)", Normal)        ) ;    

	    
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
	    

	    (* From this point, we close rw_server properly *)
	    (* No need to handle SQL because each connection is closed immediately *)
	    Unix.close Core.fd;	    
	  end
;;
