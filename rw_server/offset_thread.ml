open Types
open Types_conf
open Fdinfo
open Printf

let loop_check () =
  
  while true do

(*    Mutex.lock Files_progress.mutex_ht ;*)

    Hashtbl.iter (fun (wd, file) (date, filesize, (isfirstoffsetknown, _, error_counter), pkey_opt, created) ->
      let offset_opt =
	Files.get_offset file.f_program_pid file.f_descriptor
      in

      (* if at None then no Hashtbl update *)
      match offset_opt with
	| None ->
	  let error_counter' = error_counter + 1 in

	  (* if an offset couldn't be retrieved, then this key must
	   * be removed from the files in progress hashtable
	   * Removable is done at the second time so that time is given to a close event
	   * to be processed by core (concurrency between offset and core)
	   * The removable is actually done through the file_closed event which is forced here
	   *)
	  if error_counter' < 2 then begin
	    Hashtbl.replace Files_progress.ht
	      (wd, file)
	      (date, filesize, (isfirstoffsetknown, offset_opt, error_counter'), pkey_opt, created);
	    Log.log (("Offset. "^file.f_name^" gets a first warning."), Normal_Extra) ;
	  end else begin
	    let event =
	      match created with
		| true ->
		  (wd, [Inotify.Close_write], Int32.of_int 0, Some file.f_name)
		| false ->
		  (wd, [Inotify.Close_nowrite], Int32.of_int 0, Some file.f_name)
	    in
	    Log.log (
	      ("Offset. "^file.f_name^" gets a second and final warning. Force closing event"), Error) ;
	    Events.what_to_do event
	  end


	| Some offset ->

	  (* Add the offset_opt in the Hashtbl because of Open events in Core
	   * Also reset the error_counter.
	   *)
	  Hashtbl.replace Files_progress.ht
	    (wd, file)
	    (date, filesize, (true, offset_opt, 0), pkey_opt, created) ;

	  match (Config.get()).c_mysql with
	    | None -> ()
	    | Some _ ->
	      let sql_report =
		{
		  s_file = file ;
		  s_state =

		    (* Because the First_Known value in the SGBD is NULL,
		       instead of updating the Last Known value, this update
		       the First Known field *)
		    begin match isfirstoffsetknown with
		      | true -> SQL_LK_Offset
		      | false -> SQL_FK_Offset
		    end;
		  s_size = filesize ;
		  s_date = date ;
		  s_offset = offset_opt ;
		  s_pkey = pkey_opt ;
		  s_created = created ;
		}
	      in

	      ignore (Report.Report.report (Sql sql_report))

    ) Files_progress.ht ;

(*    Mutex.unlock Files_progress.mutex_ht ;*)
    
    Thread.delay 3.0 ;
  done
;;
