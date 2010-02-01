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



open Printf
open Inotify

open Ast
open Ast_conf

type w_info = {
  conf         : bool;
  path         : string;
  wd_father    : Inotify.wd option;
  wd_children  : Inotify.wd list
}

let report_l = ref []
let fd = Inotify.init()
let ht_iwatched = Hashtbl.create 4001
let f_accessed  = ref []


(* FUNCTIONS *)

let add_report report =
  report_l := !report_l @ [report]
;;

let get_key path_target =
  let key = ref None in 
    Hashtbl.iter (fun wd fi -> if fi.path = path_target then key := (Some wd) else ()) ht_iwatched;

    match !key with
      | None -> let err = ("error get_key with "^path_target^" not found") in Go.log err ; failwith err
      | Some k -> k
;;



let get_winfo wd =
  try
    Hashtbl.find ht_iwatched wd
  with Not_found ->
    let err = Printf.sprintf "Error in main - Hashtbl.find -> impossible to find the key: %d" (int_of_wd wd) in
    Go.log err ;
    failwith err
;;



(* Check if the watch is for a config file *)
let is_config_file wd =
  let info = get_winfo wd in
    info.conf
;;

	

let del_watch wd =

  printf "CONNARD à supprimer: %d\n" (int_of_wd wd);
  Pervasives.flush Pervasives.stdout;

  (* Check if the wd is still in the hashtable *)
  if Hashtbl.mem ht_iwatched wd then
    begin 
	
      (* Get wd's father so we can delete wd from its father's children list *)
      let wd_father_opt = (get_winfo wd).wd_father in
	  
      (* Leave it here because it used in the try and the with *)
      let wd_path = (get_winfo wd).path in

	try
(*	Hashtbl.iter (fun key _ -> printf "Wd: %d\n" (int_of_wd key)) ht_iwatched;
	  Pervasives.flush Pervasives.stdout;
*)  
	  Hashtbl.remove ht_iwatched wd;
	  
	  (* Match on wd_father_opt to know if this wd has a father *)
	  (* Delete wd from its father's children list *)
	  begin
	    match wd_father_opt with
	      | None -> printf "NULL\n"
	      | Some wd_father ->

		  (* Delete one child from the father's list *)
		  let del_child wd_father wd_child =
		    let f_father_info = get_winfo wd_father in
		    let l_new_children = List.filter (fun wd_c -> if wd_c = wd_child then false else true) f_father_info.wd_children in
		    let new_f_f_info = { f_father_info with wd_children = l_new_children } in
		      Hashtbl.replace ht_iwatched wd_father new_f_f_info
		  in
		    del_child wd_father wd;
		  let txt = sprintf "%d n'est plus enfant de %d\n" (int_of_wd wd) (int_of_wd wd_father) in
		    add_report (Log txt)
	  end;

	  let txt = sprintf "*** %s, wd = %d is not watched anymore\n" wd_path (int_of_wd wd) in
	    add_report (Log txt)

	with Failure err ->
	  begin
	    let error = sprintf "ERROR in function '%s', does the target still exist ? Here is the target concerned: '%s' et wd=%d\n" err wd_path (int_of_wd wd) in
	      prerr_endline error ; 
	      add_report (Log error) ;
	  end
    end
;;
 

   








 (* Clear the hashtable
  * Reload the config file
  * Reset the watch
  *)
(*let reinit fd =
  Hashtbl.iter (fun wd _ -> del_watch wd ) ht_iwatched;
  Hashtbl.clear ht_iwatched;
  init fd
*)

module Core =
struct

(* fd is now viewable from the outside *)
let fd = fd ;;

let add_watch path2watch wd_father_opt is_config_file =

  (* Check if the folder is not already watched *)  
  let already_exist = ref false in
  Hashtbl.iter (fun _ value -> if value.path = path2watch then already_exist := true else ()) ht_iwatched;
		  
    if !already_exist then
      begin
	let error = "Error: "^path2watch^" is already watched" in
	prerr_endline error ;
	add_report (Log error) ;
      end
    (* the folder is not alreay watched therefore we can start watching it *)
    else
      if Sys.file_exists path2watch then
	try
	  (* Start watching the wd in 2 differents ways in case it's a configuration file or not *)
	  let wd =
	    if is_config_file then
	      Inotify.add_watch fd path2watch [S_Close_write] (* 2 cases : Close_write and Ignored. it depends on the editor used to modidy it *)
	    else
	      Inotify.add_watch fd path2watch [S_Open ; S_Close_write ; S_Close_nowrite ; S_Create ; S_Delete ; S_Moved_from ; S_Moved_to]
	  in 

	    (* if the wd has a father, the entry in the hashtable is different *)
	    (match wd_father_opt with
		None ->
		  if is_config_file then
		    Hashtbl.add ht_iwatched wd {conf = true ; path = path2watch; wd_father = None; wd_children = []}
		  else
		    Hashtbl.add ht_iwatched wd {conf = false ; path = path2watch; wd_father = None; wd_children = []}
  
	      | Some wd_father ->
		  Hashtbl.add ht_iwatched wd {conf = false ; path = path2watch; wd_father = Some wd_father; wd_children = []};
  
		  (* Update a father's wd list with the new child *)
		  let add_child wd_father wd_child =
		    let f_father_info = get_winfo wd_father in
		    let new_f_f_info = { f_father_info with wd_children = wd_child::(f_father_info.wd_children) } in
		      Hashtbl.replace ht_iwatched wd_father new_f_f_info
		  in
		    add_child wd_father wd	  
	    )
	    ;
	    let txt = Printf.sprintf "*** %s is now watched, wd = %d\n" path2watch (int_of_wd wd) in
	    add_report (Log txt)
  
	with Failure err ->
	  (let error = "Error in function '"^err^"', is the name of the directory ok ? Here is the directory concerned: '"^path2watch^"'\n" in
	  prerr_endline error ;
	  add_report (Log error)
	  )
      else
	begin
	  let error = sprintf "add_watch failed : '%s' doesn't exist" path2watch in
	  add_report (Log error)
	end
;;

     

let add_watch_children l_children =

  List.iter (fun f ->
	       let folder_path = Filename.dirname f in
	       let wd_father = get_key folder_path in
	       add_watch f (Some wd_father) false
	    ) l_children
;;



(* List the subfolders of a folder *)
let ls_children path_folder =
  let ic = Unix.open_process_in ("ls -1 -R "^(Filename.quote path_folder)^" | grep :$") in
    
  let children = ref [] in
    
    (try
       while true do
	 (* returns /tmp/rw: *)
	 let folder_double_dot = input_line ic in
           (* delete the ':' *)
	 let folder = String.sub folder_double_dot 0 ((String.length folder_double_dot) -1) in	
	   children := (!children)@[folder]
       done
     with End_of_file -> let _ = Unix.close_process_in ic in print_endline "Close process"
    );
   List.tl !children
;;



let print_ht () =
    Hashtbl.iter (fun key value -> 
		    printf "\n--------------\n'%s'(%d) est le père de :\n" value.path (int_of_wd key);
		    List.iter (fun child -> printf "%d\t" (int_of_wd child)) value.wd_children
		  ) ht_iwatched 
;;
  
 

let what_to_do event conf =
  let (wd, tel, _, str_opt) = event in

  let nom =
    match str_opt with
	None -> "nothing"
      | Some x -> x
  in  
    
  let rec action type_evenement_liste is_folder =
    match type_evenement_liste with
	[] -> ()
      | type_event::q -> 

	  if not (string_of_event type_event = "ACCESS") then
	       printf "Event in progress: '%s', %s. Nom: '%s'. wd: %d\n" (string_of_event type_event) (string_of_bool is_folder) nom (int_of_wd wd)
	  ;
 
	    begin	  
	      match type_event, is_folder with

		| Isdir, _ -> action q true
		| Access, false         -> () (* déclenché avec un "more fichier.txt" *)
		| Attrib, false         -> () (* lors de création de fichier *)
		| Delete, false         -> ()
		| Modify, false         -> ()
		| Open, true            -> ()
		| Open, false           -> 
					   let folder = Filename.quote (get_winfo wd).path in

					     Printf.printf " [II] Folder: %s\n" folder;

					     let chan = Unix.open_process_in ("(lsof -w +d "^folder^") | grep REG") in
					     let l_opened_files = File_list.get chan in					       
					     let _ = Unix.close_process_in chan in

					     let l_filtered = File_list.filter conf l_opened_files in

					       Printf.printf "[II] Opened : %d\tFiltered : %d\n" (List.length l_opened_files) (List.length l_filtered);
					       
					       List.iter (
						 fun file ->
						   (* À vérifier pourquoi il y a ce test *)
						   if not (List.mem (wd,file) !f_accessed) then
						     begin
						       printf "AAAAAAAAAAAAHHHH : %s et %d\n" file.f_name (List.length l_opened_files);
						       add_report ( Notify (file.f_login^": "^nom) ) ;
						       f_accessed := (wd,file)::(!f_accessed)
						     end
							     
					       ) l_filtered
						 
		| Close_write, false    -> if Hashtbl.mem ht_iwatched wd then

		                                (* To avoid an error when updating the configuration file.
						 * It's kind of a hack.
						 * Sometimes when you change several times the configuration file,
						 * this event is triggered and the conf file loses its watch *)
		                                 if is_config_file wd then
						   begin
						     (* reinit fd; *)
						     add_report (Log "Configuration file modified and REwatch it")
						   end


		| Close_nowrite, true   -> ()
		| Close_nowrite, false  ->
		    
		    (* Call lsof to know which file stopped being accessed *)
		    
		    let folder = Filename.quote (get_winfo wd).path in
		      
		      Printf.printf " [II] Folder: %s\n" folder;
		       
		      let chan = Unix.open_process_in ("(lsof -w +d "^folder^") | grep REG") in
		      let l_opened_files = File_list.get chan in					       
		      let _ = Unix.close_process_in chan in
			
		      let l_files_in_progress = File_list.filter conf l_opened_files in
			
		      let (l_still_in_progr, l_stop_access) =
			List.partition (fun (wd', f_file) ->
					  
					  (* Check if it's in the same watch *)
					  if wd' = wd then
					    List.mem f_file l_files_in_progress					  
					  else
					    (* If not then it means it's not the same directory. Therefore we keep it *)
					    true
				       ) !f_accessed
		      in
			
			f_accessed := l_still_in_progr;
			List.iter (fun (_, f_file) -> 
				    add_report ( Notify (f_file.f_login^": "^f_file.f_name^" finished") ) ;
				  ) l_stop_access
		    



		| Create, false         -> ()
		| Create, true          -> let folder = (get_winfo wd).path in
		                           add_watch (folder^"/"^nom) (Some wd) false


		| Moved_to, true        -> let folder = ((get_winfo wd).path)^"/"^nom in
		                           let children = ls_children folder in
					     (* Watch the new folder *)
                               		    add_watch folder (Some wd) false ;
					     (* Then the folder's children *)
					     add_watch_children children

		| Moved_to, false       -> () (* Probablement les mêmes conditions que moved_from, false *)

		| Delete, true          -> let folder = (get_winfo wd).path in
                                           let wd_key = get_key (folder^"/"^nom) in
		                             del_watch wd_key

		| Moved_from, false     -> () (* Declenché quand on modifie un fichier existant et quand on renomme un fichier *)

		| Moved_from, true      -> let folder = (get_winfo wd).path in
		                           let wd_key = get_key (folder^"/"^nom) in					   
		    
					   (* Get the list of ALL the children and descendants *)
					   let rec get_all_descendants l_children =
					     List.fold_left (fun acc wd_child ->
							       (get_all_descendants (get_winfo wd_child).wd_children)@[wd_child]@acc
							    ) [] l_children
					   in
					   let children_and_descendants = get_all_descendants ((get_winfo wd_key).wd_children) in
					     
					     printf "Liste des enfants : \n";
					     List.iter (fun el -> printf "%d - " (int_of_wd el)) children_and_descendants;
					     printf "\n";
					     
					     (* Remove the watch on the children and descendants *)
					     List.iter (fun wd_child ->
							      add_report ( Log ("move_from du child : "^(get_winfo wd_child).path) ) ;
							      del_watch wd_child
						       ) children_and_descendants;
					     (* and then on the father which is the root folder moved *)
					     
					     add_report ( Log ("move_from de "^nom) ) ;
					     del_watch wd_key
					
	(*	| Move_self, _          -> if Hashtbl.mem ht_iwatched wd then
		                           begin
		                             (* Get the list of ALL the children and descendants *)
					     let rec get_all_descendants l_children =
					       List.fold_left (fun acc wd_child ->
								 (get_all_descendants (get_winfo wd_child).wd_children)@[wd_child]@acc
							      ) [] l_children
					     in
					     let children_and_descendants = get_all_descendants ((get_winfo wd).wd_children) in
					       
					       printf "Liste des enfants : \n";
					       List.iter (fun el -> printf "%d - " (int_of_wd el)) children_and_descendants;
					       printf "\n";
					       
					       (* Remove the watch on the children and descendants *)
					       List.iter (fun wd_child ->
							    if Hashtbl.mem ht_iwatched wd_child then
							      begin
								(* log ("move_self du child : "^(get_winfo wd_child).path) ; *)
								del_watch wd_child
							      end
							 ) children_and_descendants;
					   
					       (* and then on the father which is the root folder moved *)
					       (* log ("move_self de "^nom); *)
					       del_watch wd
					   end
	
		| Delete_self, false    ->   print_endline "DELETE SELF !!!!";
	*)	                             

		| Ignored, _            -> if Hashtbl.mem ht_iwatched wd then

		                                (* To avoid an error when updating the configuration file.
						 * It's kind of a hack.
						 * Sometimes when you change several times the configuration file,
						 * this event is triggered and the conf file loses its watch *)
		                                 if is_config_file wd then
						   begin
						    (* reinit (); *)
						     add_report (Log "Configuration file modified and REwatch it")
						   end
					   
		| _ -> add_report (Log ("I don't do: "^(string_of_event type_event)^", "^(string_of_bool is_folder)^" yet."))
	  end
  
  in
    action tel false;
    printf "Hashtable :%d\n" (Hashtbl.length ht_iwatched);
    Pervasives.flush Pervasives.stdout;
    
    (* Store report_l inside a temporary var so as to cleanly wipe report_l and still
    be able to return what it contained *)
    let ret_report_l = !report_l in
    report_l := [];
    ret_report_l
;;



end;;    













