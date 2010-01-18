open Ast
open Ast_conf

let get channel =
  
  let l = ref [] in    
  let regexp_cut_space= Str.regexp "[' ']+" in
  let file = {f_name="";f_path="";f_login="";f_filesize=(Int64.of_int 0);f_prog_source=""} in
    
    begin
      try
	while true do 
	  let line = input_line channel in
	    
	  let cut_line = Str.split regexp_cut_space line in
	    
	  let rec set_file cut_line column file =
	    
	    match cut_line with
		[] -> file
	      | t::q -> if column = 0 then
		  set_file q (column+1) {file with f_prog_source = t}
		    
		else if column = 1 || column = 3 || column = 4 || column = 5 || column = 7 then
		  set_file q (column+1) file
		    
		(* pseudo *)
		else if column = 2 then
		  set_file q (column+1) {file with f_login = t}
		    
		(* taille file *)
		else if column = 6 then
		  set_file q (column+1) {file with f_filesize = (Int64.of_string t)}
		    
		else
		  let path_and_filename = t^" "^(String.concat " " q)
		  in {file with f_name = (Filename.basename path_and_filename) ; f_path = (Filename.dirname path_and_filename)^"/"}		    
		       
	  in l := (set_file cut_line 0 file)::(!l)
	done
      with _ -> close_in channel
    end;
    !l





(* list unfiltred -> list filtred. The filter depends on the mode *)
let filter conf unfiltred_l =
  
  (* Check wether we're in specified mode or unwanted mode  *)      
  (* Remove all the files which the program is not in the specified_programs list *)
  match !conf.c_mode with
    | Specified_programs ->
	
	let l_prog_filtered = List.filter (fun file -> List.mem file.f_prog_source !conf.c_specified_programs) unfiltred_l in
	  
	  (* Filtre les répertoires et retourne la liste des files à insérer *)
	  List.filter (
	    fun file ->
	      (* renvoie un booléen pour dire si le path du file est l'un des répertoires surveillés *)
	      List.exists (fun directory ->
			     let rexp = Str.regexp directory in
			       Str.string_match rexp file.f_path 0
			  ) !conf.c_directories			   
	  ) l_prog_filtered		    			   
	    
	    
	    
    (* Remove all the unwanted_programs from the list *)
    | Unwanted_programs ->
	
	(* Enlève les programmes que l'on ne veut pas *)
	let l_prog_filtered = List.filter (fun file -> if List.mem file.f_prog_source !conf.c_unwanted_programs then false else true) unfiltred_l in
	  
	  (* Filtre les répertoires et retourne la liste des files à insérer *)
	  List.filter (
	    fun file ->
	      (* renvoie un booléen pour dire si le path du file est l'un des répertoires surveillés *)
	      List.exists (fun directory ->
			     let rexp = Str.regexp directory in
			       Str.string_match rexp file.f_path 0
			  ) !conf.c_directories
	  ) l_prog_filtered	
	    
