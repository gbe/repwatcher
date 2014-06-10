open Printf
open Inotify
open Unix
open Types
open Types_conf
open Report
open Sql_report

type w_info = {
  conf         : bool;
  path         : string;
  wd_father    : Inotify.wd option;
  wd_children  : Inotify.wd list
}

exception Found of Inotify.wd ;;

class core =
object(self)

  val debug_event = false
  val fd = Inotify.init ()
  val ht_iwatched = Hashtbl.create 4001


  method private _get_key path_target =
    try
      Hashtbl.iter (
	fun wd fi ->
	  if fi.path = path_target then raise (Found wd)
      ) ht_iwatched;
      None
    with Found wd -> Some wd


(* Look in the hashtable if the value exists -> bool *)
  method private _is_value path_target =
    match (self#_get_key path_target) with
      | None -> false
      | Some _ -> true


  method private _get_value wd =
    try Some (Hashtbl.find ht_iwatched wd)
    with Not_found -> None


  (* Check if the watch is for a config file *)
  method private _is_config_file wd =
  
    (* in this case, if the config file's wd can not be found,
       we just return false. It should not be a big deal
    *)
    match self#_get_value wd with
      | None      -> false
      | Some info -> info.conf


  method private _del_watch wd =

    if debug_event then begin
      printf "To be deleted: %d\n" (int_of_wd wd);
      Pervasives.flush Pervasives.stdout
    end;

    (* Check if the wd is still in the hashtable *)
    if Hashtbl.mem ht_iwatched wd then
      try
	(* Get wd's father so we can delete wd 
	 * from its father's children list *)
	match self#_get_value wd with
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
		match self#_get_value wd_father with
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

   

  method private _print_ht =
    Hashtbl.iter (fun key value -> 

      printf "\n--------------\n'%s'(%d) is the father of :\n"
	value.path (int_of_wd key);
      List.iter (fun child -> printf "%d\t" (int_of_wd child)) value.wd_children

    ) ht_iwatched;
    Pervasives.flush Pervasives.stdout


  method private _print_file file =
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
      (Fdinfo.int_of_pid file.f_program_pid)
      (Fdinfo.int_of_fd file.f_descriptor)


  method add_watch path2watch ~wd_father_opt ~is_config_file =
  
    (* Check if the folder is not already watched *)  
    if self#_is_value path2watch then
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
		match self#_get_value wd_father with
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
	  Log.log (error, Error)

    end (* eo else if *)
    else
      let error =
	sprintf "add_watch failed : '%s' doesn't exist" path2watch
      in
      Log.log (error, Error)


  method add_watch_children l_children =
    let rec r_add_watch_children l_children =
  
      match l_children with
	| [] -> ()
	| folder :: q ->
	  let father_path = Filename.dirname folder in

	  match self#_get_key father_path with
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
	      r_add_watch_children remaining

	    | Some wd_father ->
	      self#add_watch folder ~wd_father_opt:(Some wd_father) ~is_config_file:false ;
	      r_add_watch_children q
    in
    r_add_watch_children l_children


  method file_opened ?(written=false) wd name =
    match self#_get_value wd with
      | None ->
	let err =
	  sprintf "%s was opened but its wd could not be found\n" name
	in
	Log.log (err, Error)
	
      | Some father ->
      
	if debug_event then
	  Printf.printf "[II] Folder: %s\n" father.path;

	let files = Files.get father.path name in

	Mutex.lock Files_progress.mutex_ht ;
      
	List.iter (fun (file, filesize) ->
	
	  match Hashtbl.mem Files_progress.ht (wd, file) with
	    | true -> ()
	    | false ->
	      if debug_event then
		self#_print_file file;

	      let opening_date = new Date.date in
	      
	      let has_what =
		match written with
		  | false -> " has opened: "
		  | true  -> " has created: "
	      in

	      print_endline
	      (opening_date#get_str_locale^" - "^file.f_login^has_what^file.f_name);

	      Log.log (file.f_login^has_what^file.f_name, Normal);

	      let file_prepared = Report.report#prepare_data file in

	      (* *** Notifications *** *)
	      begin match written with
		| false ->
		  Report.report#notify (New_notif (file_prepared, File_Opened))
		| true ->
		  Report.report#notify (New_notif (file_prepared, File_Created))
	      end;
	      (* ********************* *)


	      (* The filesize must be overriden as unknown if the file is being created (written) *)
	      let filesize_opt =
		match written with
		  | true -> None
		  | false -> Some filesize
	      in


	      (* *** Emails *** *)
	      let tobemailed =
		{
		  m_filestate = File_Opened;
		  m_file = file_prepared;
		  m_first_offset = None;
		  m_last_offset = None;
		  m_filesize = filesize_opt;
		  m_opening_date = Some opening_date;
		  m_closing_date = None;
		}
	      in
	      begin match written with
		| false ->
		  Report.report#mail tobemailed
		| true ->
		  Report.report#mail { tobemailed with
		    m_filestate = File_Created }
	      end;
	      (* ************** *)


	      (* *** SQL *** *)
	      let sql_obj_opt =
		let sql_report =
		  {
		    s_file = file ;
		    s_state = SQL_File_Opened ;
		    s_filesize = filesize_opt ;
		    s_date = opening_date#get_str_locale ;
		    (* Desactivate the first offset when opening_file,
		     * useful when re-opening the file
		     *)
		    s_first_offset = None ;
		    s_last_offset = None ;
		    s_sql_obj = None ;
		    s_written = written ;
		  }
		in
		Report.report#sql sql_report
	      in
	      (* *********** *)
	      
	      Hashtbl.add Files_progress.ht
		(wd, file)
		(opening_date,
		 (filesize_opt, false),
		 (None, None, 0),
		 sql_obj_opt,
		 written)

	) files ;
      
	Mutex.unlock Files_progress.mutex_ht ;
		
(* eo Open, false *)


  method file_created wd name =
    self#file_opened ~written:true wd name

  method file_closed ?(written=false) wd name =

    if debug_event then begin
      match self#_get_value wd with
	| None ->
	  let err =
	    sprintf "%s has been closed (nowrite) \
		      but I can't report it because I cannot find \
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
	      ) (Fdinfo.get_fds f_file.f_program_pid))
	    with _ -> None
	  in

	  match fdinprogress with
	    | None -> ((wd2, f_file), values) :: l_stop'
	    | Some (_, fdval) ->

	      try
		(* if it's a real path then true
		 * otherwise it's something like pipe:[160367] *)
		match Sys.file_exists fdval with
		  | false -> ((wd2, f_file), values) :: l_stop'
		  | true -> l_stop'
	      with
		| _ -> ((wd2, f_file), values) :: l_stop'
      ) Files_progress.ht []
    in
					      
    Mutex.unlock Files_progress.mutex_ht ;

    List.iter (
      fun ((wd2, f_file), (opening_date, (filesize, _), (first_offset_opt, last_offset_opt, _), sql_obj_opt, _)) ->

	Mutex.lock Files_progress.mutex_ht ;	  
	Hashtbl.remove Files_progress.ht (wd2, f_file);
	Mutex.unlock Files_progress.mutex_ht ;

	let closing_date = new Date.date in
	print_endline
	  (closing_date#get_str_locale^" - "^f_file.f_login^" closed: "^f_file.f_name^" ("^(string_of_int (Fdinfo.int_of_fd f_file.f_descriptor))^")");

	Log.log (f_file.f_login^" closed: "^f_file.f_name, Normal) ;

	(* update filesize in database if written 
	 * as filesize equaled None if created/written == true *)
	let filesize =
	  match written with
	    | false -> filesize
	    | true ->
	      try
		let size =
		  (Unix.stat (f_file.f_path^f_file.f_name)).st_size
		in
		Some (Int64.of_int size)
	      with Unix_error (err_code, funct_name, fullpath) ->
		Log.log
		  ("In "^funct_name^" about "^fullpath^": "^(error_message err_code), Error);
		None
	in

	(* update last_known_offset according to filesize and written *)
	(* if file could not be read or does not exist anymore then filesize = None *)
	let overriden_last_offset_opt =
	  match last_offset_opt with
	    | None -> None (* unlikely to happen as data are read during the file_open event *)
	    | Some last_offset' ->
	      match filesize with
		| None -> last_offset_opt (* best answer we have *)
		| Some filesize' ->

		  (* fix the offset to be equal to filesize
		   * when creating the file *)
		  if filesize' > last_offset' && written then
		    filesize

		  (* Sometimes, it happens that the offset is bigger than the filesize *)
		  else if filesize' < last_offset' && not written then
		    filesize

		  else last_offset_opt
	in

	begin match Config.cfg#is_sql_activated with
	  | false -> ()
	  | true ->
	    let sql_report = {
	      s_file = f_file ;
	      s_state = SQL_File_Closed ;
	      s_filesize = filesize ;
	      s_date = closing_date#get_str_locale ;
	      s_first_offset = first_offset_opt ;
	      s_last_offset = overriden_last_offset_opt ;
	      s_sql_obj = sql_obj_opt ;
	      s_written = written ; (* better to use the written than created *)
	    }
	    in
	    ignore (Report.report#sql sql_report)
	end;

	let file_prepared = Report.report#prepare_data f_file in
	let tobemailed =
	  {
	    m_filestate = File_Closed;
	    m_file = file_prepared;
	    m_first_offset = first_offset_opt;
	    m_last_offset = overriden_last_offset_opt;
	    m_filesize = filesize;
	    m_opening_date = Some opening_date;
	    m_closing_date = Some closing_date;
	  }
	in

	Report.report#notify (New_notif (file_prepared, File_Closed));
	Report.report#mail tobemailed
    ) l_stop

(* eo file_closed, false *)


  method directory_created wd name =
    match self#_get_value wd with
      | None ->
	let err =
	  sprintf "%s has been created but I \
		   can't start watching it because I \
		   can't find its father" name
	in
	Log.log (err, Error)
	
      | Some father ->
	self#add_watch
	  (father.path^"/"^name)
	  ~wd_father_opt:(Some wd)
	  ~is_config_file:false


  method directory_moved_from wd name =
    match self#_get_value wd with
      | None ->
	let report =
	  sprintf "Error. %s has been \"moved from\" but \
                   I can't find its corresponding value in the Hashtbl. \
                   Move canceled" name
	in
	Log.log (report, Error)
	
      | Some father ->
	match self#_get_key (father.path^"/"^name) with
	  | None ->
	    Log.log ("Error. Move_from: get_key -> wd_key", Error)
	    
	  | Some wd_key ->						   
	    match self#_get_value wd_key with
	      | None ->
		Log.log ("Error: Move_from: get_value", Error)
		
	      | Some current ->
	      
		(* Get the list of ALL the children and descendants *)
		let rec get_all_descendants l_children =
		  List.fold_left (
		    fun acc wd_child ->
		      match self#_get_value wd_child with
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
		    match self#_get_value wd_child with
		      | None ->
			Log.log ("Error. What_to_do(move_from): \
				 Could not find a wd_child to delete", Error)
			
		      | Some child -> 
			Log.log ("move_from of child : "^(child.path), Normal_Extra) ;
			self#_del_watch wd_child
		) children_and_descendants ;
	      
		Log.log (("move_from of "^name), Normal_Extra) ;
	      
		(* The children's watch has been deleted,
		 * let's delete the real target *)
		self#_del_watch wd_key  
(* eo move_from, true *)


  method directory_moved_to wd name =
    match self#_get_value wd with
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
	self#add_watch path ~wd_father_opt:(Some wd) ~is_config_file:false ;
      
	(* Then the folder's children *)
	self#add_watch_children children

  method directory_deleted wd name =
    match self#_get_value wd with
      | None ->
	let err =
	  sprintf "%s has been deleted but I can't stop \
		   watching it because I can't find its father" name
	in
	Log.log (err, Error)
	
      | Some father ->
	let path = (father.path^"/"^name) in
      
	match self#_get_key path with
	  | None ->
	    let err =
	      sprintf "%s has been deleted but couldn't \
		     be stopped being watched (not found in Hashtbl)" path
	    in
	    Log.log (err, Error)
	  | Some wd_key -> self#_del_watch wd_key

  method get_fd =
    fd

  method get_debug_event =
    debug_event

end;;

let core = new core ;;

