open Types
open Types_conf
open Fdinfo
open Sql_report
open Printf
open Unix
open Files_progress



let force_closing_event (wd, file) was_being_written =

  let closing_event =
    match was_being_written with
    | true ->
      (wd, [Inotify.Close_write], Int32.of_int 0, Some file.f_name)
    | false ->
      (wd, [Inotify.Close_nowrite], Int32.of_int 0, Some file.f_name)
  in
  Log.log (
    ("Offset. "^file.f_name^" gets a second and final warning. Force closing event"), Normal_Extra) ;
  EventsDispatcher.what_to_do closing_event
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
let update_inprogress_offsets new_offset_opt key in_progress =
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
      }
;;

(* Written flag updated only if first_offset is unknown *)
(* DO NOT REMEMBER EXACTLY WHY ONLY WHEN FIRST_OFF IS UNKNOWN *)
let update_inprogress_file_to_written key in_progress =
  match !in_progress.ip_common.c_first_known_offset with
  | None ->
    in_progress :=
      { !in_progress with
	ip_common = {
	  !in_progress.ip_common with
	    c_written = true ;
	}
      };

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

(* Update the first or last known offset field into RDBMS *)
let update_sql_offset in_progress offset_type =

  let sql_report_offset = {
    sr_common = !in_progress.ip_common ;
    sr_type = offset_type;
  }
  in
  ignore (Report.report#sql
	    ~sql_obj_opt:!in_progress.ip_sql_connection
	    sql_report_offset)
;;


let could_not_get_offset (wd, file) in_progress =
  (* It happens that the offset_thread loop runs before having gotten the closing event.
   * This gives 2 loops time to get the closing event properly.
   * If after 2 loops, the event is not gotten, then closing event is forced *)

  (* Dereference of ip_offset_retrieval_errors *)
  incr !in_progress.ip_offset_retrieval_errors;

  if !(!in_progress.ip_offset_retrieval_errors) < 2 then
    Log.log (("Offset. "^file.f_name^" gets a first warning."), Normal_Extra)
  else
    force_closing_event (wd, file) !in_progress.ip_common.c_written
;;


let could_get_offset (wd, file) in_progress new_offset_opt =
  update_inprogress_offsets new_offset_opt (wd, file) in_progress;

  (* Perform the write checks only if false. True values were either
   * gotten from opening events or former false values overriden
   * by below override (update_inprogress_file_to_written) *)
  if !in_progress.ip_common.c_written = false then begin
    if is_file_being_written file in_progress then begin
      update_inprogress_file_to_written (wd, file) in_progress;
      update_sql_to_written in_progress;
    end
  end;

  (* match on the old value to know
   * which kind of sr_types must be passed
   * to the SQL query *)
  if Config.cfg#is_sql_activated then begin
    match (!in_progress).ip_common.c_first_known_offset with
    | None ->
      update_sql_offset in_progress SQL_FK_Offset ;
      update_sql_offset in_progress SQL_LK_Offset
    | Some _ -> update_sql_offset in_progress SQL_LK_Offset
  end
;;


let loop_check () =

  while true do

(*    Mutex.lock Files_progress.mutex_ht ;*)

    Hashtbl.iter (fun (wd, file) in_progress' ->

      let inprogress_ref = ref in_progress' in

      (*      Printf.printf "START - First_off: %Ld\t\tLast_off: %Ld\n"
	      (st !inprogress_ref.ip_common.c_first_known_offset)
	      (st !inprogress_ref.ip_common.c_last_known_offset);
      *)

      let new_offset_opt =
	Files.get_offset file.f_program_pid file.f_descriptor
      in

      begin
	match new_offset_opt with
	(* Hashtbl.remove will occur if offset not retrieved twice *)
	| None -> could_not_get_offset (wd, file) inprogress_ref
	| Some _ ->
	  could_get_offset (wd, file) inprogress_ref new_offset_opt;
	  (* Replace must be done exclusively if an offset could be retrieved.
	   * Otherwise, the hashtbl key gets removed when the closing event
	   * is forced and then reinserted again by the replace.
	   * According to the documentation:
	   * "If x is unbound in tbl, a binding of x to y is added to tbl." *)
	  Files_progress.htreplace
	    (wd, file)
	    !inprogress_ref
      end;

      (*
	Printf.printf "END - First_off: %Ld\t\tLast_off: %Ld\n\n"
	(st !inprogress_ref.ip_common.c_first_known_offset)
	(st !inprogress_ref.ip_common.c_last_known_offset);
      *)

    ) Files_progress.ht ;

(*    Mutex.unlock Files_progress.mutex_ht ;*)

    Thread.delay 3.0 ;
  done
;;
