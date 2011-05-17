open Types
open Fdinfo


let get pid filepath =
  let pid = pid_of_int pid in
  
  let fds =
    try
      get_fds pid
    with Unix.Unix_error (error, funct, arg) ->
      let err = Printf.sprintf "Offset. %s, in function %s, arg %s" (Unix.error_message error) funct arg in
      Log.log (err, Error) ;
      []
  in
  
  try
    let fd =
      List.find (fun fd -> fd.name = filepath) fds
    in
    Some (get_offset pid fd)

  with
    | Unix.Unix_error (error, funct, arg) ->
      let err = Printf.sprintf "\nOffset. %s, in function %s, arg %s" (Unix.error_message error) funct arg in
      Log.log ("get_offset unix error"^err, Error) ;
      None

    | Not_found ->
      Log.log ("get_offset Not_found error", Error) ;
      List.iter (fun fd -> Log.log (fd.name, Error)) fds ;
      None
;;




let loop_check () =
  
  while true do

    Hashtbl.iter (fun (wd, file) (date, _) ->
      let offset_opt =
	get file.f_program_pid (file.f_path^file.f_name)
      in

      Hashtbl.replace Files_progress.ht (wd, file) (date, offset_opt)

    ) Files_progress.ht ;
    
    Thread.delay 3.0 ;
  done
;;
