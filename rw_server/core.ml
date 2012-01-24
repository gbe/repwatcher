open Printf
open Inotify
open Unix
open Types
open Types_conf
open Report

let debug_event = false ;;

type w_info = {
  conf         : bool;
  path         : string;
  wd_father    : Inotify.wd option;
  wd_children  : Inotify.wd list
}

exception Found of Inotify.wd ;;

let fd          = Inotify.init ()
let ht_iwatched = Hashtbl.create 4001


(* FUNCTIONS *)

let get_key path_target =
  try
    Hashtbl.iter (
      fun wd fi ->
	if fi.path = path_target then raise (Found wd)
    ) ht_iwatched;
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
  try Some (Hashtbl.find ht_iwatched wd)
  with Not_found -> None
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

  if debug_event then begin
    printf "To be deleted: %d\n" (int_of_wd wd);
    Pervasives.flush Pervasives.stdout
  end;

  (* Check if the wd is still in the hashtable *)
  if Hashtbl.mem ht_iwatched wd then
    try
      (* Get wd's father so we can delete wd from its father's children list *)
      match get_value wd with
      | None ->
	  (* Due to Hashtbl.mem, this error shouldn't happened *)
	  Log.log ("Error: del_watch. Couldn't find the folder's value in the Hashtbl", Error)
      | Some folder ->
	  let wd_father_opt = folder.wd_father in	 
	  
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
				 Could not find the folder's father to delete", Error)

		  | Some f_father_info -> 
		      let l_new_children =
			List.filter (
			fun wd_c ->
			  if wd_c = wd_child then false else true
		       ) f_father_info.wd_children
		      in

		      let new_f_f_info =
			{ f_father_info with wd_children = l_new_children }
		      in
		      Hashtbl.replace ht_iwatched wd_father new_f_f_info ;

		      let report =
			sprintf "%d is no longer a child of %d\n"
			  (int_of_wd wd) (int_of_wd wd_father)
		      in
		      Log.log (report, Normal_Extra)

		in
		del_child wd_father wd
	  end;

	  let report =
	    sprintf "*** %s, wd = %d is not watched anymore\n"
	      folder.path (int_of_wd wd)
	  in
	  Log.log (report, Normal_Extra)
    with 
      Failure err ->
	let error =
	  sprintf "Error in function '%s', does the target still exist ?" err
	in
	Log.log (error, Error)
;;


   




module Core =
struct

(* fd and debug_event are now viewable from the outside *)
let fd = fd ;;
let debug_event = debug_event ;;

let add_watch path2watch wd_father_opt is_config_file =
  
  (* Check if the folder is not already watched *)  
  if is_value path2watch then
    let error = "Error: "^path2watch^" is already watched" in
    Log.log (error, Error) ;

    (* the folder is not already watched therefore we can start watching it *)
  else if Sys.file_exists path2watch then begin
    try
      (* Start watching the wd in 2 different ways 
       * in case it's a configuration file or not *)
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
      if not (Hashtbl.mem ht_iwatched wd) then begin
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

  end (* eo else if *)
  else
    let error = sprintf "add_watch failed : '%s' doesn't exist" path2watch in
    Log.log (error, Error)
;;



let rec add_watch_children l_children =
  
  match l_children with
  | [] -> ()
  | folder :: q ->
      let father_path = Filename.dirname folder in

      match get_key father_path with
      | None ->
	  let error =
	    sprintf "Oops. %s couldn't be found in the Hashtbl. \
	      Every subdirectories won't be watched. \
	      Perhaps this directory is already watched (mount --bind ?)." father_path
	  in
	  Log.log (error, Error) ;

          (* Remove every subfolders *)
	  let regexp_faulty = Str.regexp father_path in

	  let remaining =
	    List.filter (
	    fun child -> not (Str.string_match regexp_faulty child 0)
	   ) q
	  in
	  add_watch_children remaining

      | Some wd_father ->
	  add_watch folder (Some wd_father) false ;
	  add_watch_children q
;;



let print_ht () =
  Hashtbl.iter (fun key value -> 

    printf "\n--------------\n'%s'(%d) is the father of :\n"
      value.path (int_of_wd key);
    List.iter (fun child -> printf "%d\t" (int_of_wd child)) value.wd_children

  ) ht_iwatched;
  Pervasives.flush Pervasives.stdout
;;


let print_file file =
  Printf.printf
    "Name: %s\n\
     Path: %s\n\
     Login: %s\n\
     Program: %s (pid: %d)\n\
     Descr: %d\n\n"
    file.f_name
    file.f_path
    file.f_login
    file.f_program
    file.f_program_pid
    (Fdinfo.int_of_fd file.f_descriptor)
;;


let file_opened ?(created=false) wd name =

  match get_value wd with
    | None ->
      let err =
	sprintf "%s was opened but its wd could not be found\n" name
      in
      Log.log (err, Error)
	
    | Some father ->
      
      if debug_event then
	Printf.printf "[II] Folder: %s\n" father.path;

      let files = File_list.get father.path name in

      Mutex.lock Files_progress.mutex_ht ;
      
      List.iter (fun (file, filesize) ->
	
	match Hashtbl.mem Files_progress.ht (wd, file) with
	  | true -> ()
	  | false ->
	    if debug_event then
	      print_file file;

	    (* Notify right away *)
	    let date = Date.date () in

	    let has_what =
	      match created with
		| false -> " has opened: "
		| true  -> " has created: "
	    in

	    print_endline
	      (date^" - "^file.f_login^has_what^file.f_name);

	    Log.log (file.f_login^has_what^file.f_name, Normal);

	    let file_prepared = Report.prepare_data file in

	    begin match created with
	      | false ->
		ignore (Report.report (Notify (New_notif (file_prepared, File_Opened))))
	      | true ->
		ignore (Report.report (Notify (New_notif (file_prepared, File_Created))))
	    end;

	    let offset_opt =
	      Offset.get_offset file.f_program_pid (file.f_path^file.f_name)
	    in

	    (* if the file is being created then the
	     * filesize is unknown for the moment
	     *)
	    let filesize =
	      match created with
		| false -> Some filesize
		| true -> None
	    in

	    let sql_report = 
	      {
		s_file = file ;
		s_state = SQL_File_Opened ;
		s_size = filesize ;
		s_date = date ;
		s_offset = offset_opt ;
		s_pkey = None ;
		s_created = created ;
	      }
	    in

	    match Report.report (Sql sql_report) with
	      | Nothing -> () (* could be triggered by an SQL error *)
	      | PrimaryKey pkey ->
		let isfirstoffsetknown =
		  match offset_opt with
		    | None -> false
		    | Some _ -> true
		in

		Hashtbl.add Files_progress.ht
		  (wd, file)
		  (date,
		   filesize,
		   (isfirstoffsetknown, offset_opt, 0),
		   pkey,
		   created)

      ) files ;
      
      Mutex.unlock Files_progress.mutex_ht ;
		
(* eo Open, false *)
;;


let file_created wd name =
  file_opened ~created:true wd name
;;



let file_closed ?(written=false) wd name =

  if debug_event then begin
    match get_value wd with
      | None ->
	let err =
	  sprintf "%s has been closed (nowrite) \
		      but I can't report it because I can't find\
		      its wd info" name
	in
	Log.log (err, Error)
	
      | Some folder ->
	let path_quoted = Filename.quote folder.path in       
	Printf.printf "[II] Folder: %s\n" path_quoted ;
  end;

  Mutex.lock Files_progress.mutex_ht ;
      
  (* Return the list of the files which stopped being accessed *)
  let l_stop =
    Hashtbl.fold (
      fun (wd2, f_file) values l_stop' ->

	(* Return new infos on a specific fdnum.
	 * If the process is already closed, a Sys_error is triggered
	 * by Fdinfo.get_fds
	 *)
	let fdinprogress =
	  try
	    Some (List.find (fun (fd, fdval) ->
	      fd = f_file.f_descriptor
	    ) (Fdinfo.get_fds (Fdinfo.pid_of_int f_file.f_program_pid)))
	  with _ -> None
	in

	match fdinprogress with
	  | None -> ((wd2, f_file), values) :: l_stop'
	  | Some (_, fdval) ->

	    try
	      (* if it's a real path then true otherwise it's something like pipe:[160367] *)
	      match Sys.file_exists fdval with
		| false -> ((wd2, f_file), values) :: l_stop'
		| true -> l_stop'
	    with
	      | _ -> ((wd2, f_file), values) :: l_stop'
    ) Files_progress.ht []
  in
					      
  Mutex.unlock Files_progress.mutex_ht ;

  List.iter (
    fun ((wd2, f_file), (d, filesize, (isoffsetknown, offset, err_counter), pkey, created)) ->

      Mutex.lock Files_progress.mutex_ht ;	  
      Hashtbl.remove Files_progress.ht (wd2, f_file);
      Mutex.unlock Files_progress.mutex_ht ;

      let date = Date.date () in
      print_endline (date^" - "^f_file.f_login^" closed: "^f_file.f_name^" ("^(string_of_int (Fdinfo.int_of_fd f_file.f_descriptor))^")");

      Log.log (f_file.f_login^" closed: "^f_file.f_name, Normal) ;

      (* update filesize in database if written *)
      let filesize =
	match written with
	  | false -> filesize
	  | true ->
	    let size =
	      (Unix.stat (f_file.f_path^f_file.f_name)).st_size
	    in
	    Some (Int64.of_int size)
      in

      (* update last known offset according to filesize if written is true *)
      let offset =
	match written with
	  | false -> offset
	  | true ->
	    match offset with
	      | None -> None
	      | Some offset' ->
		match filesize with
		  | None -> assert false
		  | Some filesize' ->
		    if filesize' > offset' then
		      Some filesize'
		    else
		      offset
      in

      let sql_report = {
	s_file = f_file ;
	s_state = SQL_File_Closed ;
	s_size = filesize ;
	s_date = date ;
	s_offset = offset ;
	s_pkey = Some pkey ;
	s_created = created ;
      }
      in	  

      ignore (Report.report (Sql sql_report));
      let file_prepared = Report.prepare_data f_file in
      ignore (Report.report (Notify (New_notif (file_prepared, File_Closed))));

  ) l_stop

(* eo file_closed, false *)
;;




let directory_created wd name =
  match get_value wd with
    | None ->
      let err =
	sprintf "%s has been created but I \
			    can't start watching it because I \
			    can't find its father" name
      in
      Log.log (err, Error)
	
    | Some father ->
      add_watch (father.path^"/"^name) (Some wd) false
;;


let directory_moved_from wd name =
  match get_value wd with
    | None ->
      let report =
	sprintf "Error. %s has been \"moved from\" but \
                         I can't find its corresponding value in the Hashtbl. \
                         Move canceled" name
      in
      Log.log (report, Error)
	
    | Some father ->
		
      match get_key (father.path^"/"^name) with
	| None ->
	  Log.log ("Error. Move_from: get_key -> wd_key", Error)
	    
	| Some wd_key ->						   
	  match get_value wd_key with
	    | None ->
	      Log.log ("Error: Move_from: get_value", Error)
		
	    | Some current ->
	      
	      (* Get the list of ALL the children and descendants *)
	      let rec get_all_descendants l_children =
		List.fold_left (
		  fun acc wd_child ->
		    match get_value wd_child with
		      | None -> []
		      | Some child ->
			(get_all_descendants child.wd_children)@[wd_child]@acc
		) [] l_children
	      in
	      let children_and_descendants =
		get_all_descendants current.wd_children
	      in
				       	
	      (* Remove the watch on the children and descendants *)
	      List.iter (
		fun wd_child ->
		  match get_value wd_child with
		    | None ->
		      Log.log ("Error. What_to_do(move_from): \
				 Could not find a wd_child to delete", Error)
			
		    | Some child -> 
		      Log.log ("move_from of child : "^(child.path), Normal_Extra) ;
			      del_watch wd_child
	      ) children_and_descendants ;
	      
	      Log.log (("move_from of "^name), Normal_Extra) ;
	      
              (* The children's watch has been deleted,
               * let's delete the real target *)
	      del_watch wd_key
			  
(* eo move_from, true *)
;;


let directory_moved_to wd name =
  match get_value wd with
    | None ->
      let report =
	sprintf "%s has been \"moved from\" but I \
			 can't find its father. Move cancel" name
      in
      Log.log (report, Error)
	
    | Some father ->
      let path = (father.path)^"/"^name in
      
      let children =
	(* Exception raised if the list returned by Dirs.ls is empty.
	 * This shouldn't happen because 'folder' should be at least returned
	 * If raised, it means the folder couldn't be opened by Unix.opendir
	 *)
	try
	  List.tl (Dirs.ls path [])
	with Failure _ ->
	  let error =
	    "For some reasons, '"^path^"' could not be browsed \
                    while doing a move_to"
	  in
	  Log.log (error, Error);
	  []
      in
      
      (* Watch the new folder *)
      add_watch path (Some wd) false ;
      
      (* Then the folder's children *)
      add_watch_children children
;;

let directory_deleted wd name =
  match get_value wd with
    | None ->
      let err =
	sprintf "%s has been deleted but I can't stop \
		 watching it because I can't find its father" name
      in
      Log.log (err, Error)
	
    | Some father ->
      let path = (father.path^"/"^name) in
      
      match get_key path with
	| None ->
	  let err =
	    sprintf "%s has been deleted but couldn't \
		     be stopped being watched (not found in Hashtbl)" path
	  in
	  Log.log (err, Error)
	| Some wd_key -> del_watch wd_key
;;

end;; (* eo module *)
