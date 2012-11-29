open Types
open Types_conf
open Fdinfo
open Unix

let extract line =
  let r = Str.regexp "[^(]+(\\([^)]+\\))" in
  match (Str.string_match r line 0) with
    | true -> Str.matched_group 1 line
    | false ->
      Log.log ("Program name could not be extracted", Error) ;      
      "?"
;;

let get path name =

  let conf = Config.get () in
  let pids = get_pids () in
  let fullpath = path^"/"^name in

  List.fold_left (fun acc pid ->
    try

      let pid_int = int_of_pid pid in
      let pid_str = string_of_int pid_int in

      let uid = (Unix.stat ("/proc/"^pid_str)).st_uid in
      let login = (Unix.getpwuid uid).pw_name in

      (* Filtered if user ignored *)
      match (List.mem login conf.c_watch.w_ignore_users) with
	| true -> acc
	| false ->
	  begin

	    let fds = get_fds pid in
	    
	    (* Test if this processus opened this file.
	     * The processus may have opened several times the same file,
	     * thus List.find_all and not List.find.
	     * If not found, then acc is returned through the try/with *)
	    let file_fds =
	      fst (
		List.split (
		  List.find_all (fun (fd, fd_filename) ->
		    fullpath = fd_filename
		  ) fds
		)
	      )
	    in    

	    let size = (Unix.stat fullpath).st_size in
	    
	    let prog_c = open_in ("/proc/"^pid_str^"/stat") in
	    let line = input_line prog_c in
	    close_in_noerr prog_c;

	    let prog = extract line in
      
	    match conf.c_mode with
	      | (Specified_programs, specified_programs) ->
		if List.mem prog specified_programs then
		  List.fold_left (fun acc' file_fd ->
		    ({
		      f_name = name ;
		      f_path = path^"/" ;
		      f_login = login ;
		      f_program = prog ;
		      f_program_pid = pid ;
		      f_descriptor = file_fd ;
		    }, (Int64.of_int size)) :: acc'
		  ) acc file_fds
		else
		  acc
		
	      | (Unwanted_programs, unwanted_programs) ->
		if List.mem prog unwanted_programs then
		  acc
		else
		  List.fold_left (fun acc' file_fd ->
		    ({
		      f_name = name ;
		      f_path = path^"/" ;
		      f_login = login ;
		      f_program = prog ;
		      f_program_pid = pid ;
		      f_descriptor = file_fd ;
		    }, (Int64.of_int size)) :: acc'
		  ) acc file_fds
	  end
    with
      | _ -> acc
	
  ) [] pids
;;


let get_offset pid fd =

  try
    Some (get_infos pid fd).offset
  with _ -> None
;;


(*
let get_offset_old pid filepath =
  let pid = pid_of_int pid in
  
  let fds =
    try
      get_fds pid
    with
      | Unix.Unix_error (error, funct, arg) ->
	let err =
	  Printf.sprintf "Offset. %s, in function %s, arg %s"
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
*)
