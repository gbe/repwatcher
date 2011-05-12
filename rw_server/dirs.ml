open Types;;

let reg_hidden = Str.regexp "[.]";;


(* List the subfolders of a folder.
 * The head of the list returned is the folder given in arg
 * Sub branches not parsed if they're ignored
 *)
  let rec ls folder ignored_directories =
  
  let dh_opt = ref None in
  
  begin
    try
      (* Check if the given folder exists in argument *)
      match Sys.is_directory folder with
      | true ->  dh_opt := Some (Unix.opendir folder)
      | false -> ()

    with
  | Sys_error err -> Log.log (err, Error)

 (* If triggered here then the siblings will be able to be read *)
  | Unix.Unix_error (err, fun_name, dir) ->
      Log.log (("Unix error in "^fun_name^" function: "^(Unix.error_message err)^" - "^dir), Error)

  end;
  
  
  match !dh_opt with
  | None -> []
  | Some dh ->
      let l = ref [folder] in
      begin
	try
	  while true do
	    let entry = Unix.readdir dh in
	    try
	      let fullpath = (folder^"/"^entry) in

	      (* Test if it's a file or a folder *)
	      match Sys.is_directory fullpath with
	      | true ->
		  begin
		    match entry with
		    | ("." | "..") -> ()
		    | _            ->
		      (* Browse if it's not a hidden directory *)
		      match Str.string_match reg_hidden entry 0 with
			| true -> () (* it's a hidden directory *)
			| false ->
			(* Stop browsing the subdirectories if they're ignored *)
			  let to_be_ignored =
			    try
			      ignore (List.find (fun ign_dir_reg -> Str.string_match ign_dir_reg fullpath 0) ignored_directories);
			      true
			    with Not_found -> false
			  in
			  begin match to_be_ignored with
			    | true -> () (* ignored *)
			    | false -> l := (!l)@(ls fullpath ignored_directories)
			  end
		  end
	      | false -> ()
	    with Sys_error err -> Log.log (err, Error)
		
	  done;
	with
	| End_of_file -> Unix.closedir dh

	| Unix.Unix_error (err, fun_name, file_or_dir) ->
	    (* If this error is triggered then the next siblings
	     * won't be watched because never read.
	     * Every error triggered here should be caught elsewhere *)

	    Log.log (("Unix error in "^fun_name^" function: "^(Unix.error_message err)^" - "^file_or_dir^"\tPlease report this. It shouldn't happened"), Error)
      end;
      !l
;;
