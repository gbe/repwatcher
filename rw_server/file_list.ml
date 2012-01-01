open Types
open Types_conf
open Fdinfo
open Unix

let get path name =

  let conf = Config.get () in
  let pids = Fdinfo.get_pids () in
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
	    
	    let prog_c = open_in ("/proc/"^pid_str^"/comm") in
	    let prog = input_line prog_c in
	    close_in_noerr prog_c;
      
	    match conf.c_mode with
	      | (Specified_programs, specified_programs) ->
		if List.mem prog specified_programs then
		  List.fold_left (fun acc' file_fd ->
		    ({
		      f_name = name ;
		      f_path = path^"/" ;
		      f_login = login ;
		      f_program = prog ;
		      f_program_pid = pid_int ;
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
		      f_program_pid = pid_int ;
		      f_descriptor = file_fd ;
		    }, (Int64.of_int size)) :: acc'
		  ) acc file_fds
	  end
    with
      | _ -> acc
	
  ) [] pids
;;
