open Types
open Types_conf


let get channel =

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
    try  
      while true do
	let line = input_line channel in
	
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
    with _ -> close_in channel

  end ;
  !files

;;








(*
let get channel =
  
  let l = ref [] in    
  let regexp_cut_space = Str.regexp "[' ']+" in
  let regexp_quote     = Str.regexp "'" in
  let regexp_quote2    = Str.regexp "&apos;" in

  let file = {
    f_name = "" ;
    f_path = "" ;
    f_login = "" ;
    f_filesize = (Int64.of_int 0) ;
    f_program = "" ;
    f_program_pid = -1 ;
  } in
  
  begin
    try

      while true do 
	let line     = Str.global_replace regexp_quote "&apos;" (input_line channel) in
	let cut_line = Str.split regexp_cut_space line in

	let rec set_file cut_line column file =  
	  match cut_line with
	  | [] -> file
	  | t::q -> if column = 0 then
	      set_file q (column + 1) {file with f_program = t}

	  else if column = 1 then
	    set_file q (column + 1) {file with f_program_pid = (int_of_string t)}
		
	  else if List.mem column [3 ; 4 ; 5 ; 7] then
	    set_file q (column + 1) file
	      
	      (* login *)
	  else if column = 2 then
	    set_file q (column + 1) {file with f_login = t}
	      
	      (* file size *)
	  else if column = 6 then
	    set_file q (column + 1) {file with f_filesize = (Int64.of_string t)}
	      
	  else
	    begin
	      let path_and_filename = Str.global_replace regexp_quote2 "'" (t^" "^(String.concat " " q)) in

	      {file with f_name = (Filename.basename path_and_filename) ; f_path = (Filename.dirname path_and_filename)^"/"}
	    end
	      
	in l := (set_file cut_line 0 file)::(!l)
      done
    with _ -> close_in channel
  end;
  !l
;;
*)



(* list unfiltered -> list filtered. The filter depends on the mode and if the user is ignored *)
let filter unfiltered_l =
  
  let conf = Config.get() in
    
  (* Remove all the files according to the specified or unwanted mode from the config file *)
  let lprogs_filtered =
    match conf.c_mode with
    | (Specified_programs, specified_programs) ->
	List.filter (fun file -> List.mem file.f_program specified_programs) unfiltered_l
	    
    (* Remove all the unwanted_programs from the list *)
    | (Unwanted_programs, unwanted_programs) ->	  	  
	List.filter (fun file -> if List.mem file.f_program unwanted_programs then false else true) unfiltered_l
	    
  in
    
    (* Remove all the files accessed by one of the ignored users *)
    List.filter (fun file ->
		   not (List.mem file.f_login conf.c_watch.w_ignore_users)
		) lprogs_filtered
;;	    
