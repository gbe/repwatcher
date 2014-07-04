open Types
open Types_conf
open Fdinfo
open Sql_report
open Printf
open Unix
open Files_progress


(* Update the count errors or remove the entry in the ht
 * if too many errors *)
let update_ht_offset_retrieval_errors (wd, file) in_progress =
  (* dereference ip_offset_retrieval_errors *)
  incr !in_progress.ip_offset_retrieval_errors;

  (* if an offset couldn't be retrieved, then this key must
   * be removed from the files in progress hashtable
   * Removal is done at the second time so that time is given to a close event
   * to be processed by core (concurrency between offset and core)
   * The removal is actually done through the file_closed event which is forced here
   *)
  if !(!in_progress.ip_offset_retrieval_errors) < 2 then begin
    Hashtbl.replace
      Files_progress.ht
      (wd, file)
      !in_progress;

    Log.log (("Offset. "^file.f_name^" gets a first warning."), Normal_Extra) ;

  end else begin
    let event =
      match !in_progress.ip_common.c_written with
	| true ->
	  (wd, [Inotify.Close_write], Int32.of_int 0, Some file.f_name)
	| false ->
	  (wd, [Inotify.Close_nowrite], Int32.of_int 0, Some file.f_name)
    in
    Log.log (
      ("Offset. "^file.f_name^" gets a second and final warning. Force closing event"), Normal_Extra) ;
    Events.what_to_do event
  end
;;


(* Written value given by an opening event cannot be trusted
 * New check based on filesizes comparison *)
let is_file_being_written file in_progress =
  if !in_progress.ip_common.c_written = true &&
    !in_progress.ip_filesize_checked_again = true then
    true

  else
    match !in_progress.ip_common.c_filesize with
    | None -> assert false (* case when written = true *)
    | Some ofilesize ->
      let nfilesize =
	try
	  Int64.of_int (Unix.stat (file.f_path^"/"^file.f_name)).st_size
	with Unix_error (err, "stat", _) ->
	  Log.log ("Offset_thread: Error could not stat the file "^file.f_name, Error) ;
	  Int64.of_int 0
      in

      (* Actual test to know if the written bool must be overridden *)
      if nfilesize > ofilesize then begin
	Log.log (file.f_name^" is actually being written, overriding the 'written' value to true", Normal_Extra);
	true
      end else
	(* Must be 'written' and not false as the nfilesize could be 0
	 * because it could not be read in the above test
	 * therefore the written value could be true *)
	!in_progress.ip_common.c_written
;;



(* Add the offset_opt in the Hashtbl because of Open events in Core
 * update the first_known_offset and last_known_offset with the same value if first_known_offset is None
 * update the last_known_offset if first_known_offset is a Some
 *
 * Also reset the error_counter and set to true the offset_check_again as it
 * is necessary to be true at this point since if it used to be at false,
 * the check has been performed above
 *)
let update_offsets_in_ht new_offset_opt key in_progress =
  begin
    match !in_progress.ip_common.c_first_known_offset with
  | None ->
    (* The last_known_offset is necessarily equal to the first_known_offset *)
    in_progress :=
      { !in_progress with
	ip_common = {
	  !in_progress.ip_common with
	    c_first_known_offset = new_offset_opt ;
	    c_last_known_offset = new_offset_opt ;
	};
	ip_filesize_checked_again = true ;
	ip_offset_retrieval_errors = ref 0 ;
      }

  | Some _ ->
    (* Only the last_known_offset must be updated *)
    in_progress :=
      { !in_progress with
	ip_common = {
	  !in_progress.ip_common with
	    c_last_known_offset = new_offset_opt
	}
      };
  end;

  Hashtbl.replace
    Files_progress.ht
    key
    !in_progress
;;

(* Written flag updated only if first_offset is unknown *)
(* DO NOT REMEMBER EXACTLY WHY ONLY WHEN FIRST_OFF IS UNKNOWN *)
let update_file_to_written key in_progress =
  match !in_progress.ip_common.c_first_known_offset with
    | None ->
      in_progress :=
 	{ !in_progress with
	  ip_common = {
	    !in_progress.ip_common with
	      c_written = true ;
	  }
	};
      Hashtbl.replace
	Files_progress.ht
	key
	!in_progress

    | Some _ -> ()
;;

(* If the file in progress was flagged as not written while it
 * is actually being written, then the 'written' flag in the RDBMS
 * must be turned on *)
let update_sql_to_written in_progress =

  let written_report = {
    sr_common = !in_progress.ip_common ;
    sr_type = SQL_Switch_On_Created ;
  }
  in
  ignore (Report.report#sql
	    ~sql_obj_opt:!in_progress.ip_sql_connection
	    written_report)
;;

let update_sql in_progress first_off_backup =

  let sql_report_offset =
    {
      sr_common = !in_progress.ip_common ;
      sr_type =
	(* Update the first of last known offset field into RDBMS *)
	begin match first_off_backup with
	| None -> SQL_FK_Offset
	| Some _ -> SQL_LK_Offset
	end;
    }
  in
  ignore (Report.report#sql
	    ~sql_obj_opt:!in_progress.ip_sql_connection
	    sql_report_offset)
;;

let loop_check () =

  while true do

(*    Mutex.lock Files_progress.mutex_ht ;*)

    Hashtbl.iter (fun (wd, file) in_progress' ->

      let inprogress_ref = ref in_progress' in

      let new_offset_opt =
	Files.get_offset file.f_program_pid file.f_descriptor
      in

      match new_offset_opt with
	| None ->
	  (* Update the count errors or remove the entry in the ht
	  * if too many errors *)
	  update_ht_offset_retrieval_errors
	    (wd, file)
	    inprogress_ref

	| Some _ ->
	  (* Perform the write checks only if false. True values were either
	   * gotten from opening events or former false values overriden
	   * by below override (update_file_to_written) *)
	  if !inprogress_ref.ip_common.c_written = false then begin
	    if is_file_being_written file inprogress_ref then begin
	      update_file_to_written (wd, file) inprogress_ref;
	      update_sql_to_written inprogress_ref;
	    end
	  end;

	  (* save the value for using it into update_sql, to know
	   * which kind of sr_types must be passed to the SQL query *)
	  let first_off_backup = !inprogress_ref.ip_common.c_first_known_offset in
	  update_offsets_in_ht new_offset_opt (wd, file) inprogress_ref;

	  match (Config.cfg)#is_sql_activated with
	  | false -> ()
	  | true -> update_sql inprogress_ref first_off_backup

    ) Files_progress.ht ;

(*    Mutex.unlock Files_progress.mutex_ht ;*)

    Thread.delay 3.0 ;
  done
;;
