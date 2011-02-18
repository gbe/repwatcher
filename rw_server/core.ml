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



open Printf
open Inotify

open Types
open Types_conf
open Report

let debug_event = false;;

type w_info = {
  conf         : bool;
  path         : string;
  wd_father    : Inotify.wd option;
  wd_children  : Inotify.wd list
}

exception Found of Inotify.wd;;

let fd          = Inotify.init()
let ht_iwatched = Hashtbl.create 4001


(* FUNCTIONS *)

let get_key path_target =
  try
    Hashtbl.iter (
      fun wd fi ->
	if fi.path = path_target then raise (Found wd)
    ) ht_iwatched;
    let err = ("error get_key with "^path_target^" not found") in
    Log.log (err, Error) ;
    None
  with Found wd -> Some wd
;;



(* Look in the hashtable if the value exists -> bool *)
let is_value path_target =
  try
    Hashtbl.iter (
      fun wd fi ->
	if fi.path = path_target then raise (Found wd)
    ) ht_iwatched;
    false
  with Found _ -> true
;;


let get_value wd =
  try
    Some (Hashtbl.find ht_iwatched wd)
  with Not_found ->
    let err = Printf.sprintf "Error in main - Hashtbl.find -> \
	impossible to find the key: %d" (int_of_wd wd) in
    Log.log (err, Error) ;
    None
;;



(* Check if the watch is for a config file *)
let is_config_file wd =
  
(* in this case, if the config file's wd can not be found,
   we just return false. It should not be a big deal
 *)
  match get_value wd with
  | None      -> false
  | Some info -> info.conf
;;



let del_watch wd =
  if debug_event then
    begin
      printf "To be deleted: %d\n" (int_of_wd wd);
      Pervasives.flush Pervasives.stdout
    end;

  (* Check if the wd is still in the hashtable *)
  if Hashtbl.mem ht_iwatched wd then
    try
      (* Get wd's father so we can delete wd from its father's children list *)
      match get_value wd with
      | None -> Log.log ("Error: del_watch. Could not find the wd to delete", Error)
      | Some value ->
	  let wd_father_opt = value.wd_father in	 
	  
	  Hashtbl.remove ht_iwatched wd;
	  
	  (* Match on wd_father_opt to know if this wd has a father *)
	  (* Delete wd from its father's children list *)
	  begin
	    match wd_father_opt with
	    | None -> ()
	    | Some wd_father ->
		
		(* Delete one child from the father's list *)
		let del_child wd_father wd_child =
		  match get_value wd_father with
		  | None ->
		      Log.log ("Error: del_watch. \
				 Could not find the wd's father to delete", Error)

		  | Some f_father_info -> 
		      let l_new_children =
			List.filter (fun wd_c ->
			  if wd_c = wd_child then false else true
				    )f_father_info.wd_children
		      in

		      let new_f_f_info =
			{ f_father_info with wd_children = l_new_children }
		      in
		      Hashtbl.replace ht_iwatched wd_father new_f_f_info

		in
		del_child wd_father wd;
		
		let report =
		  sprintf "%d n'est plus enfant de %d\n"
		    (int_of_wd wd) (int_of_wd wd_father)
		in
		Log.log (report, Normal_Extra)
	  end;

	  let report =
	    sprintf "*** %s, wd = %d is not watched anymore\n"
	      value.path (int_of_wd wd)
	  in
	  Log.log (report, Normal_Extra)
    with 
      Failure err ->
	let error =
	  sprintf "ERROR in function '%s', does the target still exist ? \
	    Here is the wd_target concerned: %d\n" err (int_of_wd wd)
	in
	prerr_endline error ;
	Log.log (error, Error)
;;


   




module Core =
struct

(* fd is now viewable from the outside *)
let fd = fd ;;

let add_watch path2watch wd_father_opt is_config_file =
  
  (* Check if the folder is not already watched *)  
  if is_value path2watch then
    let error = "Error: "^path2watch^" is already watched" in
    Log.log (error, Error) ;
    
    (* the folder is not already watched therefore we can start watching it *)
  else
    if Sys.file_exists path2watch then
      try
	(* Start watching the wd in 2 differents ways in case it's a configuration file or not *)
	let wd =
	  if is_config_file then
	    Inotify.add_watch fd path2watch [S_Close_write]

	  else
	    Inotify.add_watch fd path2watch
	      [S_Open ;
	       S_Close_write ;
	       S_Close_nowrite ;
	       S_Create ;
	       S_Delete ;
	       S_Moved_from ;
	       S_Moved_to]
	in 
	
	(* If this inode is not already watched but with a different path.
	 *  It can occur if mount --bind is used and both folders are set to be watched
	 *)
	if not (Hashtbl.mem ht_iwatched wd) then
	  begin
	    (* if the wd has a father, the entry in the hashtable is different *)
	    (match wd_father_opt with
	    | None ->
		if is_config_file then
		  Hashtbl.add ht_iwatched wd
		    {
		     conf = true ;
		     path = path2watch ;
		     wd_father = None ;
		     wd_children = []
		   }
		else
		  Hashtbl.add ht_iwatched wd
		    {
		     conf = false ;
		     path = path2watch ;
		     wd_father = None ;
		     wd_children = []
		   }
		    
	    | Some wd_father ->
		Hashtbl.add ht_iwatched wd
		  {
		   conf = false ;
		   path = path2watch ;
		   wd_father = Some wd_father ;
		   wd_children = []
		 };
		
		(* Update a father's wd list with the new child *)
		let add_child wd_father wd_child =
		  match get_value wd_father with
		  | None ->
		      Log.log ("Exception triggered in add_watch. Unknown wd", Error)

		  | Some f_father_info ->
		      Hashtbl.replace ht_iwatched wd_father
			{
			 f_father_info with
			 wd_children = wd_child :: (f_father_info.wd_children)
		       }
		in
		add_child wd_father wd	  
	    );
	    let txt =
	      Printf.sprintf "*** %s is now watched, wd = %d\n"
		path2watch (int_of_wd wd)
	    in
	    Log.log (txt, Normal_Extra)
	  end
	    
      with 
      | Failure err ->
	  let error =
	    "Error in function '"^err^"', is the name \
	of the directory ok ? Here is \
	    the directory concerned: '"^path2watch^"'\n"
	  in
	  prerr_endline error ;
	  Log.log (error, Error)
    else
      let error = sprintf "add_watch failed : '%s' doesn't exist" path2watch in
      Log.log (error, Error)
;;



let add_watch_children l_children =
  
  List.iter (
  fun f ->
    let folder_path = Filename.dirname f in
    
    match get_key folder_path with
    | None ->
	Log.log ("Oops. Can't start watching a children list because \
		   I can't find their wd's father based on his path", Error)
		   
    | Some wd_father ->
	add_watch f (Some wd_father) false 
	  
 ) l_children
;;



let print_ht () =
  Hashtbl.iter (fun key value -> 
		  printf "\n--------------\n'%s'(%d) est le père de :\n" value.path (int_of_wd key);
		  List.iter (fun child -> printf "%d\t" (int_of_wd child)) value.wd_children
	       ) ht_iwatched 
;;



let what_to_do event =
  let (wd, tel, _, str_opt) = event in

  let name =
    match str_opt with
    | None -> "nothing"
    | Some x -> x
  in  
    
  let rec action type_evenement_list is_folder =
    match type_evenement_list with
    | [] -> ()
    | type_event::q -> 
	  
	if debug_event then
	  begin
	    if not (string_of_event type_event = "ACCESS") then
	      printf "Event in progress: '%s', %s. Name: '%s'. wd: %d\n"
		(string_of_event type_event)
		(string_of_bool is_folder)
		name
		(int_of_wd wd)
	  end;
	  
	begin match type_event, is_folder with	
	| Isdir, _ -> action q true

	| Access, false -> () (* déclenché avec un "more fichier.txt" *)
	| Attrib, false -> () (* lors de création de fichier *)
	| Delete, false -> ()
	| Modify, false -> ()
	| Open, true -> ()

	| Open, false -> 
	    begin
	      match get_value wd with
	      | None ->
		  let err =
		    sprintf "%s was opened but its wd could not be found\n" name
		  in
		  Log.log (err, Error)
		    
	      | Some value ->
		  let folder = Filename.quote value.path in
		  
		  if debug_event then
		    Printf.printf "[II] Folder: %s\n" folder;
		  
		  let chan =
		    Unix.open_process_in ("(lsof -w +d "^folder^") | grep REG")
		  in
		  
		  let l_opened_files = File_list.get chan in					       
		  ignore (Unix.close_process_in chan);
		  
		  let l_filtered = File_list.filter l_opened_files in
		  
		  if debug_event then
		    Printf.printf "[II] Opened : %d\tFiltered : %d\n"
		      (List.length l_opened_files) (List.length l_filtered);
		  
		  List.iter (
		  fun file ->
		    
		    (* This test is here because without it
		     * we could be notified 3 times for the same thing *)
		    if not (Hashtbl.mem Files_progress.ht (wd,file)) then
		      begin
			if debug_event then
			  printf "AAAAAAAAAAAAHHHH : Filename: %s et Filesize: %s et name: %s\n"
			    file.f_name (Int64.to_string file.f_filesize) name;
			
			let date = Date.date () in
			print_endline (date^" - "^file.f_login^" has opened: "^file.f_name);

			Log.log (file.f_login^" is downloading: "^file.f_name, Normal);
			Report.report ( Sql (file, File_Opened, date) );
			Report.report ( Notify (New_notif (file, File_Opened)) );
			Hashtbl.add Files_progress.ht (wd,file) date
		      end
		   ) l_filtered

	    end (* eo Open, false *)
					      

	| Close_write, false ->
	    if Hashtbl.mem ht_iwatched wd then
	      
	      (* To avoid an error when updating the configuration file.
	       * It's kind of a hack.
	       * Sometimes when you change several times the configuration file,
	       * this event is triggered and the conf file loses its watch *)
	      if is_config_file wd then
		(* reinit fd; *)
		Log.log ("Configuration file modified and REwatch it", Normal_Extra)


	| Close_nowrite, true -> ()


	| Close_nowrite, false ->
	    begin
	      match get_value wd with
	      | None ->
		  let err =
		    sprintf "%s has been closed (nowrite) \
		      but I can't report it because I can't find\
		      its wd info" name
		  in
		  Log.log (err, Error)
					  
	      | Some value ->
		  let folder = Filename.quote value.path in
					      
		  if debug_event then
		    Printf.printf "[II] Folder: %s\n" folder;
					      
		  (* Call lsof to know which file stopped being accessed *)
		  let chan =
		    Unix.open_process_in ("(lsof -w +d "^folder^") | grep REG")
		  in

		  let l_opened_files = File_list.get chan in
		  ignore (Unix.close_process_in chan);
		  let l_files_in_progress = File_list.filter l_opened_files in

                  (* Return the list of the files which stopped being accessed *)
		  let l_stop    =
		    Hashtbl.fold (
		    fun (wd2, f_file) _ l_stop' ->
		      (* if wd2 is wd's child and is not in progress anymore *)
                      if ( wd = wd2 &&
			   not (List.mem f_file l_files_in_progress) ) then
			
			(wd2, f_file)::l_stop'
		      else
			l_stop'
                   ) Files_progress.ht []
		  in
					      
		  List.iter (
		  fun (wd2, f_file) ->
                    Hashtbl.remove Files_progress.ht (wd2, f_file);

		    let date = Date.date() in
		    print_endline (date^" - "^f_file.f_login^" closed: "^f_file.f_name);

		    Log.log (f_file.f_login^" finished downloading: "^f_file.f_name, Normal) ;
		    Report.report ( Sql (f_file, File_Closed, date) ) ;
		    Report.report ( Notify (New_notif (f_file, File_Closed) )) ;
		   ) l_stop
	    end (* eo Close_nowrite, false *)


	      | Create, false -> ()

	      
	      | Create, true ->
	      	  begin
		    match get_value wd with
		    | None ->
			let err =
			  sprintf "%s has been created but I \
			    can't start watching it because I \
			    can't find its father" name
			in
			Log.log (err, Error)
					       
		    | Some value ->
	      		let folder = value.path in
		        add_watch (folder^"/"^name) (Some wd) false
		  end

					   
	      | Moved_to, true ->
	      	  begin
	      	    match get_value wd with
		    | None ->
			let report =
			  sprintf "%s has been \"moved from\" but I \
			    can't find its father. Move cancel" name
			in
			Log.log (report, Error)
					   
		    | Some value ->
			let folder = (value.path)^"/"^name in
		        let children =
			  (* Exception raised if the list returned by Dirs.ls is empty.
			   * This shouldn't happen because 'folder' should be at least returned
			   * If raised, it means the folder couldn't be opened by Unix.opendir *)
			  try
			    List.tl (Dirs.ls folder [])
			  with Failure _ ->
			    let error = "For some reasons, '"^folder^"' could not be opened while doing a move_to" in
			    Log.log (error, Error);
			    []
			in
			
			(* Watch the new folder *)
                        add_watch folder (Some wd) false ;
					       
			(* Then the folder's children *)
			add_watch_children children
		  end
		    

	      (* Probably the same conditions than moved_from, false *)
	      | Moved_to, false -> ()

		  
	      | Delete, true ->
                  let err =
		    sprintf "%s has been deleted but I can't stop \
		      watching it because I can't find its father" name
		  in
		  begin
		    match get_value wd with
		    | None -> Log.log (err, Error)

		    | Some value  ->
			let folder = value.path in

			match get_key (folder^"/"^name) with
			| None -> Log.log (err, Error)
			| Some wd_key -> del_watch wd_key						      
		  end


	      (* Triggered when an existing file is modified
	       * and when a file is renamed *)
	      | Moved_from, false -> ()

		  
	      | Moved_from, true ->
	      	  begin
	      	    match get_value wd with
		    | None ->
			let report =
			  sprintf "Error. %s has been \"moved from\" but I can't find its corresponding value in the Hashtbl. Move canceled" name
			in
			Log.log (report, Error)
			  
		    | Some value1  ->
			let folder = value1.path in
			
			match get_key (folder^"/"^name) with
			| None ->
			    Log.log ("Error. Move_from: get_key -> wd_key", Error)

			| Some wd_key ->						   
			    match get_value wd_key with
			    | None ->
				Log.log ("Error: Move_from: get_value", Error)

			    | Some value2 ->
						       
				(* Get the list of ALL the children and descendants *)
				let rec get_all_descendants l_children =
				  List.fold_left (
				  fun acc wd_child ->
				    match get_value wd_child with
				    | None -> []
				    | Some value3 ->
					(get_all_descendants value3.wd_children)@[wd_child]@acc
				 ) [] l_children
				in
				let children_and_descendants = get_all_descendants value2.wd_children in
				
				(* printf "Liste des enfants : \n";
				   List.iter (fun el -> printf "%d - " (int_of_wd el)) children_and_descendants;
				   printf "\n";
				 *)
				
				(* Remove the watch on the children and descendants *)
				List.iter (
				fun wd_child ->
				  match get_value wd_child with
				  | None ->
				      Log.log ("Error. What_to_do(move_from): \
						 Could not find a wd_child to delete", Error)
			 
				  | Some value4 -> 
				      Log.log ("move_from du child : "^(value4.path), Normal_Extra) ;
				      del_watch wd_child
			       ) children_and_descendants
				;
				Log.log (("move_from de "^name), Normal_Extra) ;
				del_watch wd_key

		  end (* eo move_from, true *)

             (* When IGNORED is triggered it means the wd
	      * is not watched anymore. Therefore, we need
	      * to take this wd out of the Hashtbl *)		    
	      | Ignored, _  ->

		  if Hashtbl.mem ht_iwatched wd then

		    (* To avoid an error when updating the configuration file.
		     * It's kind of a hack.
		     * Sometimes when you change several times the configuration file,
		     * this event is triggered and the conf file loses its watch *)
		    begin
		      if is_config_file wd then
			(* reinit (); *)
			Log.log ("Configuration file modified and REwatch it", Normal_Extra) ;
		      
		      print_ht ();
		    end
					   
	      | _ ->
		  Log.log ("I don't do: "^(string_of_event type_event)^", "
			   ^(string_of_bool is_folder)^" yet.", Normal_Extra)
	end (* eo match type_event *)
  
  in
  action tel false;
  Pervasives.flush Pervasives.stdout
;; (* eo what_to_do *)


end;; (* eo module *)












