open Types
open Types_conf
open Fdinfo
open Sql_report
open Printf
open Unix
open Files_progress

let loop_check () =
  
  while true do

(*    Mutex.lock Files_progress.mutex_ht ;*)

    Hashtbl.iter (fun (wd, file) in_progress ->

      let new_offset_opt =
	Files.get_offset file.f_program_pid file.f_descriptor
      in

      match new_offset_opt with
	| None ->
	  let error_counter' =
	    in_progress.ip_offset_retrieval_errors + 1
	  in

	  (* if an offset couldn't be retrieved, then this key must
	   * be removed from the files in progress hashtable
	   * Removal is done at the second time so that time is given to a close event
	   * to be processed by core (concurrency between offset and core)
	   * The removal is actually done through the file_closed event which is forced here
	   *)
	  if error_counter' < 2 then begin
	    Hashtbl.replace Files_progress.ht
	      (wd, file)
	      { in_progress with
		ip_offset_retrieval_errors = error_counter' };
	    Log.log (("Offset. "^file.f_name^" gets a first warning."), Normal_Extra) ;
	  end else begin
	    let event =
	      match in_progress.ip_common.c_written with
		| true ->
		  (wd, [Inotify.Close_write], Int32.of_int 0, Some file.f_name)
		| false ->
		  (wd, [Inotify.Close_nowrite], Int32.of_int 0, Some file.f_name)
	    in
	    Log.log (
	      ("Offset. "^file.f_name^" gets a second and final warning. Force closing event"), Normal_Extra) ;
	    Events.what_to_do event
	  end


	| Some _ ->

	  (* Overwrite the written value given by an event 
	   * during the opening by a new value based on
	   * filesizes comparison *)
	  let written' = 
	    if in_progress.ip_common.c_written = false &&
	      in_progress.ip_filesize_checked_again = false then begin
	      match in_progress.ip_common.c_filesize with
		| None -> assert false (* case when written = true *)
		| Some ofilesize ->
		  let nfilesize =
		    try
		      Int64.of_int (Unix.stat (file.f_path^"/"^file.f_name)).st_size
		    with Unix_error (err, "stat", _) ->
		      Log.log ("Offset_thread: Error could not stat the file "^file.f_name, Error) ;
		      Int64.of_int 0
		  in

		  (* the file in progress is being written,
		   * the written value must be overridden *)
		  if nfilesize > ofilesize then begin
		    Log.log (file.f_name^" is actually being written, overriding the 'written' value to true", Normal_Extra);
		    true
		  end else
		    (* Must be 'written' and not false as the nfilesize could be 0
		     * because it could not be read in the above test
		     * therefore the written value could be true *)
		    in_progress.ip_common.c_written
	    end
	    else
	      in_progress.ip_common.c_written
	  in

	  (* Add the offset_opt in the Hashtbl because of Open events in Core
	   * update the first_known_offset if it is None
	   * update the last_known_offset if first_known_offset is a Some

	   * Also reset the error_counter and set to true the offset_check_again as it
	   * is necessary to be true at this point since if it used to be at false,
	   * the check has been performed above
	   *)
	  
	  begin match in_progress.ip_common.c_first_known_offset with
	  | None ->
	    (* The last_known_offset is necessarily equal to the first_known_offset *)
	    Hashtbl.replace Files_progress.ht
	      (wd, file)
	      { in_progress with
		ip_common = {
		  in_progress.ip_common with
		    c_first_known_offset = new_offset_opt ;
		    c_last_known_offset = new_offset_opt ;
		    c_written = written' ;
		};
		ip_filesize_checked_again = true ;
		ip_offset_retrieval_errors = 0 ;
	      }
	  | Some _ ->
	    (* Only the last_known_offset must be updated *)
	    Hashtbl.replace Files_progress.ht
	      (wd, file)
	      { in_progress with
		ip_common = {
		  in_progress.ip_common with
		    c_last_known_offset = new_offset_opt
		}
	      }
	  end;

	  match (Config.cfg)#is_sql_activated with
	    | false -> ()
	    | true ->

	      (* If the file in progress was flagged as not created while it
	       * is actually being written, then the 'created' flag in the RDBMS
	       * must be turned on *)
	      if in_progress.ip_common.c_written = false &&
		written' = true then begin
		let sql_report_created =
		  {
		    s_file = file ;
		    s_state = SQL_Switch_On_Created ;
		    s_filesize = in_progress.ip_common.c_filesize ;
		    s_date = in_progress.ip_common.c_opening_date#get_str_locale ;
		    s_first_offset = None ; (* Not used thus None *)
		    s_last_offset = None ; (* Not used thus None *)
		    s_written = true ;
		  }
		in
		ignore (Report.report#sql
			  ~sql_obj_opt:in_progress.ip_sql_connection
			  sql_report_created)
	      end;

	      let sql_report_offset =
		{
		  s_file = file ;
		  s_state =

		    (* Because the First_Known value in the RDBMS is NULL,
		       instead of updating the Last Known value, this updates
		       the SQL First Known field *)
		    begin match in_progress.ip_common.c_first_known_offset with
		      | None -> SQL_FK_Offset
		      | Some _ -> SQL_LK_Offset
		    end;
		  s_filesize = in_progress.ip_common.c_filesize ;
		  s_date = in_progress.ip_common.c_opening_date#get_str_locale ;
		  s_first_offset =
		    (* First offset not modified if already existing *)
		    begin match in_progress.ip_common.c_first_known_offset with
		    | None -> new_offset_opt ;
		    | Some _ -> in_progress.ip_common.c_first_known_offset ;
		    end;
		  s_last_offset = new_offset_opt ;
		  s_written = in_progress.ip_common.c_written ;
		}
	      in
	      ignore (Report.report#sql
			~sql_obj_opt:in_progress.ip_sql_connection
			sql_report_offset)

    ) Files_progress.ht ;

(*    Mutex.unlock Files_progress.mutex_ht ;*)
    
    Thread.delay 3.0 ;
  done
;;
