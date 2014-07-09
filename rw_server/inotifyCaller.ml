open Printf
open Inotify
open Unix
open Types
open Types_conf
open Report
open Sql_report
open Files_progress

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

  (* TODO: remove _ if public *)
  method _get_key path_target =
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

  (* TODO:remove _ if public *)
  method _get_value wd =
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


  (* TODO: remove _ if public *)
  method _del_watch wd =

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

  (* TODO: remove _ if public *)
  method  _print_file file =
    Printf.printf
    "Name: %s\n\
     Path: %s\n\
     Login: %s\n\
     Username: %s\n\
     Program: %s (pid: %d)\n\
     Descr: %d\n\n"
      file.f_name
      file.f_path
      file.f_unix_login
      file.f_username
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

  method get_fd =
    fd

  method get_debug_event =
    debug_event

end;;

let core = new core ;;
