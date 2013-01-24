open Types;;
open Report;;


let wait_pipe_from_child_process () =
  let bufsize = 1024 in
  let buf = String.create bufsize in
  
  while true do
    let recv = Unix.read Pipe.child2father#get_toread buf 0 bufsize in
    
    if recv > 0 then begin
      let data = String.sub buf 0 recv in
      let com = (Marshal.from_string data 0 : Types.com_net2main) in
      match com with
	| Ask_current_accesses ->

	  Mutex.lock Files_progress.mutex_ht ;

	  let l_current =
	    Hashtbl.fold (fun (_,file) (date,_,_,_,_) ret ->
	      let file_prepared = Report.prepare_data file in

	      ret@[ (
		{ file_prepared with
		  f2_name = (Txt_operations.escape_for_notify file_prepared.f2_name)
		}, date)
		  ]
	    ) Files_progress.ht []
	  in

	  Mutex.unlock Files_progress.mutex_ht ;

	    
	  (* Sent to Report.notify only if necessary *)
	  if List.length l_current > 0 then
	    ignore (Report.report (Notify (Old_notif l_current)))

	| Types.Log log -> Log.log log

	| Exit_on_error error ->
	    Log.log (error, Error);
	    Unix.kill (Unix.getpid()) Sys.sigabrt
    end;
  done
;;