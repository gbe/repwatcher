open Printf
open Inotify
open Unix
open Types
open InotifyCaller
open Report
open Sql_report
open Files_progress


let directory_created wd name =
  match InotifyCaller.core#get_value wd with
  | None ->
    let err =
      sprintf "%s has been created but I \
	       can't start watching it because I \
	       can't find its father" name
    in
    Log.log (err, Error)

  | Some father ->
    InotifyCaller.core#add_watch
      (father.path^"/"^name)
      ~wd_father_opt:(Some wd)
;;


let directory_moved_from wd name =
  match InotifyCaller.core#get_value wd with
  | None ->
    let report =
      sprintf "Error. %s has been \"moved from\" but \
               I can't find its corresponding value in the Hashtbl. \
               Move canceled" name
    in
    Log.log (report, Error)

  | Some father ->
    match InotifyCaller.core#get_key (father.path^"/"^name) with
    | None ->
      Log.log ("Error. Move_from: get_key -> wd_key", Error)

    | Some wd_key ->
      match InotifyCaller.core#get_value wd_key with
      | None ->
	Log.log ("Error: Move_from: get_value", Error)

      | Some current ->

	(* Get the list of ALL the children and descendants *)
	let rec get_all_descendants l_children =
	  List.fold_left (
	    fun acc wd_child ->
	      match InotifyCaller.core#get_value wd_child with
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
	    match InotifyCaller.core#get_value wd_child with
	    | None ->
	      Log.log ("Error. What_to_do(move_from): \
			Could not find a wd_child to delete", Error)

	    | Some child ->
	      Log.log ("move_from of child : "^(child.path), Normal_Extra) ;
	      InotifyCaller.core#del_watch wd_child
	) children_and_descendants ;

	Log.log (("move_from of "^name), Normal_Extra) ;

	(* The children's watch has been deleted,
	 * let's delete the real target *)
	InotifyCaller.core#del_watch wd_key
(* eo move_from, true *)
;;

let directory_moved_to wd name =
  match InotifyCaller.core#get_value wd with
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
    InotifyCaller.core#add_watch
      path
      ~wd_father_opt:(Some wd);

    (* Then the folder's children *)
    InotifyCaller.core#add_watch_children children
;;

let directory_deleted wd name =
  match InotifyCaller.core#get_value wd with
  | None ->
    let err =
      sprintf "%s has been deleted but I can't stop \
               watching it because I can't find its father" name
    in
    Log.log (err, Error)

  | Some father ->
    let path = (father.path^"/"^name) in

    match InotifyCaller.core#get_key path with
    | None ->
      let err =
	sprintf "%s has been deleted but couldn't \
		 be stopped being watched (not found in Hashtbl)" path
      in
      Log.log (err, Error)
    | Some wd_key -> InotifyCaller.core#del_watch wd_key
;;
