open Types
open Types_conf
open Fdinfo
open Unix

let extract line =
  let r = Str.regexp "[^(]+(\\([^)]+\\))" in
  match (Str.string_match r line 0) with
    | true -> Str.matched_group 1 line
    | false ->
      Log.log ("Program name could not be extracted from \""^line^"\"", Error) ;      
      "?"
;;

let get path name =

  let conf = Config.get () in
  let fullpath = path^"/"^name in

  let pids =
    try
      Fdinfo.get_pids ()
    with Fdinfo_unix_error (err, funct, ppids) ->
      let msg =
	Printf.sprintf "%s failed: %s. %s is probably already closed"
	  funct
	  (error_message err)
	  fullpath
      in
      Log.log (msg, Normal_Extra);

      match ppids with
	| Ffd _ -> assert false
	| Ppid pids -> pids
  in

  List.fold_left (fun acc pid ->
    try

      let pid_int = int_of_pid pid in
      let pid_path = "/proc/"^(string_of_int pid_int) in
      let fd_path = pid_path^"/fd" in
      
      let access_rights =
	try
	  Unix.access fd_path [R_OK; X_OK; F_OK];
	  true
	with _ -> false
      in

      let uid = (Unix.stat pid_path).st_uid in
      let login = (Unix.getpwuid uid).pw_name in

      (* must have access rights on /proc/pid/fd and user not ignored *)
      match (access_rights && not (List.mem login conf.c_watch.w_ignore_users)) with 
	| false -> acc
	| true ->
	  begin

	    let fds = 
	      try
		Fdinfo.get_fds pid
	      with Fdinfo_unix_error (err, funct, ffds) ->
		Log.log ((funct^" failed: "^(error_message err)), Normal_Extra);
		match ffds with
		  | Ppid _ -> assert false
		  | Ffd fds -> fds
	    in
	    
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

	    (* Could be 0 if EACCES "opendir" is triggered *)
	    match (List.length file_fds) with
	      | 0 -> acc
	      | _ ->
		let size = (Unix.stat fullpath).st_size in
		  
		let prog_c = open_in (pid_path^"/stat") in
		let line = input_line prog_c in
		close_in prog_c;

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
      | Unix_error (err, "stat", _) ->
	Log.log ("Error could not stat the file "^name, Error) ;
	acc
      | Unix_error (err, "getpwuid", _) ->
	Log.log ("Error could not getpwuid the user for the file "^name, Error) ;
	acc
      | Not_found ->
	Log.log ("Could not getpwuid the user who opened the file "^name^" while his/her ID is known", Error) ;
	acc

  ) [] pids
;;


let get_offset pid fd =

  try
    Some (get_infos pid fd).offset
  with
    | Fdinfo_parse_error ->
      let msg =
	Printf.sprintf "Fdinfo_parse_error raised for pid: %d. Should not happen."
	  (int_of_pid pid)
      in
      Log.log (msg, Error) ;
      None

    | Fdinfo_sys_error err ->
      Log.log ("Fdinfo_sys_error: '"^err^"'. Should be a file already closed. Otherwise it's an error", Normal_Extra);
      None
;;
