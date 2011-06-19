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
		  begin match entry with
		    | ("." | "..") -> ()
		    | _            ->
		      (* Browse if it's not a hidden directory *)
		      match Str.string_match reg_hidden entry 0 with
			| true -> () (* it's a hidden directory *)
			| false ->
			  (* Stop browsing the subdirectories if they're ignored *)
			  let to_be_ignored =
			    try
			      ignore (List.find (fun ign_dir_reg ->
				Str.string_match ign_dir_reg fullpath 0
			      ) ignored_directories);
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



(* filer the directories with the ignored ones and return their children *) 
let filter_and_get_children directories ignore_directories =   
  
  let rem_slash l_directories =
    List.map (
    fun dir ->
      (* If the directory name ends with a '/' then it is deleted *)
      if String.get dir ((String.length dir) -1) = '/' then
	String.sub dir 0 ((String.length dir)-1)
      else dir
   ) l_directories
  in
  
  let directories = ref (rem_slash directories) in
  let ign_dirs_without_slash = rem_slash ignore_directories in
  
  let filter directories l_regexp =
    List.filter (
    fun dir ->
      try
	if Sys.is_directory dir then						
	  try
	    ignore (List.find (fun reg  -> Str.string_match reg dir 0) l_regexp);
	    false (* false = taken off the list *)
	  with Not_found -> true
	else
	  false
	    
      (* No such file or directory *)
      with Sys_error e ->
	let error = "Error: "^e in 
	prerr_endline error ;
	Log.log (error, Error) ;
	false
   ) directories
  in


  (*
   * Filter
   * - if it doesn't exist
   * - the subdirectories of an ignored one like /home/dest/Ftp/ignored/dir (set to be watched) and /home/dest/Ftp/ignored (set to be ignored)
   * /home/dest/Ftp/ignored/dir is taken off of the list
   *)
  let l_regexp_ign_dirs_slash_ending =
    List.map (
      fun ign_dir ->
	(* Str.quote escapes special chars which causes problems: $^.*+?[]
	 * Refer to Str manpage
	 *)
	Str.regexp ((Str.quote ign_dir)^"/")
    ) ign_dirs_without_slash
  in
  directories := filter !directories l_regexp_ign_dirs_slash_ending;


  (* This dollar is used for the regexp to keep *only* the folder "test - etc" (derivative names) in the following example :
     - /home/dest/test - etc
     - /home/dest/test      <---  this one is the ignored folder
   * Without it both directories would be ignored
   *
   * This regexp also takes off of the list an entry
   * which is the exact same one if it's set to be watched and ignored (what stupid people can do) :
     - to be watched: /home/dest/Ftp
     - to be ignored: /home/dest/Ftp
   *)
  let l_regexp_ign_dirs_dollar =
    List.map (
      fun ign_dir ->
	Str.regexp ((Str.quote ign_dir)^"$")
    ) ign_dirs_without_slash
  in
  directories := filter !directories l_regexp_ign_dirs_dollar;
  

  let children =
    List.fold_left (
    fun dirs2watch dir ->
      let children_of_a_branch =
	try
	  List.tl (ls dir l_regexp_ign_dirs_dollar)
	with Failure _ -> []
      in
      children_of_a_branch@dirs2watch
   ) [] !directories
  in
  
  (!directories, children)
;;
