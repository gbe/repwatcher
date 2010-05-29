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



(* TO DO

- Voir pour mettre en surveillance des fichiers et pas seulement des dossiers
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
 * - Put a watch on it
 *
 * Then watch the directories
 *)
let init () =

  let load_and_watch_config () =
    
    if Sys.file_exists config_file then
      begin
	(* Get the configuration file *)
	let conf = Config.parse config_file in
	  (* Watch it *)
	  Core.add_watch config_file None true;
	  conf
      end
    else
      failwith "Config file doesn't exist"
  in



  (* Set the watch on the directories *) 
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
       * - is not set to be watched AND also ignored. Only a stupid people can do that
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
	      List.filter (fun dir_child ->
			     (* - if the exception is triggered, then it means that the directories ignored
			      * have nothing to do with this one
			      * 
			      * - This code is the same that above but without the test Sys.is_directory because I consider that the answer is true.
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
  in
    
  let conf = load_and_watch_config () in
    watch_dirs conf.c_directories conf.c_ignore_directories;
    conf
;;


(* Fonction main *)
let _ =
  
  (* Verifie que le programme est lance en root seulement *)
(*  if (Unix.getuid()) <> 0 then
    begin
      print_endline "This program has to be executed as root\n";
      exit 1
    end;
*)


  (* Load the configuration file and then watch the directories given in it *)
  let conf = init () in

  let fd = match conf.c_notify_rem with
    | true  ->
	Report.report (Log ("Start server for remote notifications", Level_2)) ;
	Unix.fork()
    | false -> -1
  in
      
      
    match fd with
      | 0 -> if conf.c_notify_rem then Ssl_server.run Report.tor
      | _ ->
	  begin
	    
	    Core.print_ht ();
	    
	    Pervasives.flush Pervasives.stdout;
	    
	    Report.report (Notify "Repwatcher is watching youuu ! :)") ;
	    Report.report ( Log ("Repwatcher is watching youuu ! :)", Level_1) ) ;    
	    
	    while true do
	      
	      let _,_,_ = Unix.select [ Core.fd ] [] [] (-1.) in
	      let event_l = Inotify.read Core.fd in
		
		List.iter (fun event -> Core.what_to_do event) event_l;
	    done;
	    
	    Unix.close Core.fd
	  end
;;
