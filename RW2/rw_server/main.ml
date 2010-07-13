(*
    This file is part of Repwatcher.

    Repwatcher is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Repwatcher is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Repwatcher; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

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
let watch_dirs conf_directories ignore_directories =   
  
  let rem_slash l_directories = List.map (
    fun dir ->
      (* If the directory name ends with a '/' then it is deleted *)
      if String.get dir ((String.length dir) -1) = '/' then
	String.sub dir 0 ((String.length dir)-1)
      else dir
   ) l_directories
  in
  
  let directories                   = ref (rem_slash conf_directories)                  in
  let ignore_directories_clean_name =     rem_slash ignore_directories                  in
  
  let regexp_ignore_directories     = List.map Str.regexp ignore_directories_clean_name in
  
  
  (* Filter if the directory in the config file
   * - exists
   * - is not set to be watched AND also ignored. Only someone stupid can do that
   *)
  directories := List.filter (fun dir ->
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
      Report.report (Log (error, Level_1));
      false
			     ) !directories
      ;
  
  
  let children =
    List.fold_left (
    fun dirs2watch dir ->
      
      let dir_children = Core.ls_children dir in
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
  
  List.iter (fun dir -> Core.add_watch dir None false) !directories;
  Core.add_watch_children children
;;
    




(* Fonction main *)
let _ =
  
  (* Load and watch the configuration file *)
  let conf = load_and_watch_config () in


  (* Test if we can successfully connect to the SGBD
   * if we don't, then we exit right now
   * This is added because the program crashed long after starting running it
   * at the very moment a SQL transaction was needed
   * With this, we know from the start if (at least this part) it goes right or wrong.
   *)  
  begin
    try
      Mysqldb.connect();
      Mysqldb.disconnect()
    with Mysql.Error msg ->
      Report.report (Log (msg, Level_1));
      let error_msg = Printf.sprintf "%s\nPlease, make sure the SQL password is correct.\n" msg in
      failwith error_msg
  end;
  
  (* watch the directories given in the config file *)
  watch_dirs conf.c_directories conf.c_ignore_directories;

  
  let fd = match conf.c_notify_rem with
    | true  ->
	Report.report (Log ("Start server for remote notifications", Level_2)) ;
	Unix.fork()
    | false -> -1
  in
      
     
    match fd with
      | 0 ->
	  if conf.c_notify_rem then begin
	    Ssl_server.run Pipe.tor Pipe.tow2
	  end

      | _ ->
	  begin
	  if conf.c_notify_rem then begin
	    ignore (Thread.create Pipe_listening.wait_pipe_from_child_process ())
	  end;
	    
(*	    Core.print_ht (); *)
	       
	    Report.report ( Notify ( Info_notif "Repwatcher is watching youuu ! :)" )  ) ;
	    Report.report ( Log   ("Repwatcher is watching youuu ! :)", Level_1)       ) ;    

	    
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
	    
	    (* Close the fd used to log *)
	    Report.close_fd2log();
	  end
;;
