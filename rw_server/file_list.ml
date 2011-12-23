open Types
open Types_conf
open Fdinfo
open Unix

let get2 fullpath =

  let conf = Config.get() in
  let pids = Fdinfo.get_pids () in

  List.fold_left (fun acc pid ->
    try

      let pid_int = int_of_pid pid in
      let pid_str = string_of_int pid_int in

      let uid = (Unix.stat ("/proc/"^pid_str)).st_uid in
      let login = (Unix.getpwuid uid).pw_name in

      (* Filter if user ignored *)
      match (List.mem login conf.c_watch.w_ignore_users) with
	| true -> acc
	| false ->
	  begin

	    let fds = get_fds pid in
	    
	    (* Test if this processus opened this file.
	     * The processus may have opened several times the same file,
	     * thus List.find_all and not List.find.
	     * If not found, then acc is returned through the try/with *)

	    (* TO DO: réutiliser les fds pour les injecter dans les fichiers ouverts *)
	    let _ =
	      List.find_all (fun (fd, fd_filename) ->
		fullpath = fd_filename
	      ) fds
	    in    
      
	    let size = (Unix.stat fullpath).st_size in
	    
	    let prog_c = open_in ("/proc/"^pid_str^"/comm") in
	    let prog = input_line prog_c in
	    close_in_noerr prog_c;
      
	    match conf.c_mode with
	      | (Specified_programs, specified_programs) ->
		if List.mem prog specified_programs then
		  {
		    f_name = (Filename.basename fullpath) ;
		    f_path = (Filename.dirname fullpath)^"/" ;
		    f_login = login ;
		    f_filesize = (Int64.of_int size) ;
		    f_program = prog ;
		    f_program_pid = pid_int ;
		  } :: acc
		else
		  acc
		
	      | (Unwanted_programs, unwanted_programs) ->
		if List.mem prog unwanted_programs then
		  acc
		else
		  {
		    f_name = (Filename.basename fullpath) ;
		    f_path = (Filename.dirname fullpath)^"/" ;
		    f_login = login ;
		    f_filesize = (Int64.of_int size) ;
		    f_program = prog ;
		    f_program_pid = pid_int ;
		  } :: acc
	  end
    with
      | _ -> acc
	
  ) [] pids
;;


let get cmd =

  let file = ref {
    f_name = "" ;
    f_path = "" ;
    f_login = "" ;
    f_filesize = (Int64.of_int 0) ;
    f_program = "" ;
    f_program_pid = -1 ;
  } in

  let files = ref [] in

  (*
   * process number
   * program name
   * program identity (login)
   * filesize
   * file fullpath
   *)

  (*
    p4399
    cvlc
    Ldest
    s367147008
    n/home/ftp/Movies/FNL.avi
  *)
  let r_process = Str.regexp "^p" in
  let r_prog_name = Str.regexp "^c" in
  let r_id = Str.regexp "^L" in
  let r_size = Str.regexp "^s" in
  let r_fullpath = Str.regexp "^n" in

  begin

    let channel_opt = ref None in

    try
      let chan = Unix.open_process_in cmd in
      
      (* this is a copy to not make a match None/Some at input_line *)
      channel_opt := Some chan ;
      
      while true do
	
       let line = input_line chan in
	
	if Str.string_match r_process line 0 then
	  file := { !file with f_program_pid = int_of_string (Str.string_after line 1) }
	    
	else if Str.string_match r_prog_name line 0 then
	  file := { !file with f_program = (Str.string_after line 1) }
	    
	else if Str.string_match r_id line 0 then
	  file := { !file with f_login = (Str.string_after line 1) }
	    
	else if Str.string_match r_size line 0 then
	  file := { !file with f_filesize = Int64.of_string (Str.string_after line 1) }

	else if Str.string_match r_fullpath line 0 then
	  let fullpath = Str.string_after line 1 in
	  file := { !file with
	    f_name = (Filename.basename fullpath) ;
	    f_path = (Filename.dirname fullpath)^"/"
	  } ;
	  
	  files := !file :: !files
	else
	  assert false
      done
    with _ ->
      match !channel_opt with
	| None -> ()
	| Some chan ->
	  try
	    ignore (Unix.close_process_in chan)
	  with _ -> ()
  end ;
  !files

;;



(* list unfiltered -> list filtered. 
 *The filter depends on the mode and if the user is ignored *)
let filter unfiltered_l =
  
  let conf = Config.get() in
    
  (* Remove all the files according to 
   * the specified or unwanted mode from the config file *)
  let lprogs_filtered =
    match conf.c_mode with
    | (Specified_programs, specified_programs) ->
	List.filter (fun file ->
	  List.mem file.f_program specified_programs
	) unfiltered_l
	    
    (* Remove all the unwanted_programs from the list *)
    | (Unwanted_programs, unwanted_programs) ->	  	  
	List.filter (fun file ->
	  not (List.mem file.f_program unwanted_programs)
	) unfiltered_l	    
  in
    
  (* Remove all the files accessed by one of the ignored users *)
  List.filter (fun file ->
    not (List.mem file.f_login conf.c_watch.w_ignore_users)
  ) lprogs_filtered
;;	    
