open Types
open Fdinfo
open Printf

let get_offset pid filepath =
  let pid = pid_of_int pid in
  
  let fds =
    try
      Fdinfo.get_fds pid
    with
      | Unix.Unix_error (error, funct, arg) ->
	let err =
	  sprintf "Offset. %s, in function %s, arg %s"
	    (Unix.error_message error) funct arg
	in
	Log.log (err, Error) ;
	[]

      | Fdinfo_parse_error -> 
	Log.log ("Offset. Error while parsing data from /proc", Error) ;
	[]
  in
  
  try
    let fd = fst (List.find (fun (_, fd_path) -> fd_path = filepath) fds) in
    Some ((get_infos pid fd).offset)
  with
    | Not_found ->
      Log.log (("Offset. "^filepath^" not_found in /proc."), Error) ;
      List.iter (fun (_, fdname) -> Log.log (fdname, Error)) fds ;
      None
;;




let loop_check () =
  
  while true do

    Mutex.lock Files_progress.mutex_ht ;

    Hashtbl.iter (fun (wd, file) (date, (isfirstoffsetknown, _), sql_pkey) ->
      let offset_opt =
	get_offset file.f_program_pid (file.f_path^file.f_name)
      in

      (* if at None then no Hashtbl update *)
      match offset_opt with
	| None -> ()
	| Some offset ->

	  (* Add the offset_opt in the Hashtbl because of Open events in Core *)
	  if Hashtbl.mem Files_progress.ht (wd, file) then begin
	    Hashtbl.replace Files_progress.ht
	      (wd, file) (date, (true, offset_opt), sql_pkey) ;

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
		s_date = date ;
		s_offset = offset_opt ;
		s_pkey = Some sql_pkey
	      }
	    in

	    ignore (Report.Report.report (Sql sql_report))
	  end

    ) Files_progress.ht ;

    Mutex.unlock Files_progress.mutex_ht ;
    
    Thread.delay 3.0 ;
  done
;;
