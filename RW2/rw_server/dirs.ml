(* List the subfolders of a folder.
 * The head of the list returned is the folder given in arg *)
let rec ls folder =
  
  let dh_opt = ref None in
  
  begin try
    (* Teste si le répertoire donné en argument existe *)
    match Sys.is_directory folder with
    | true ->  dh_opt := Some (Unix.opendir folder)
    | false -> ()
  with Sys_error _ -> ()
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
	      (* Teste si c'est un fichier ou un dossier *)
	      match Sys.is_directory fullpath with
	      | true ->
		  begin
		    match entry with
		    | ("." | "..") -> ()
		    | _            -> l := (!l)@(ls fullpath)
		  end
	      | false -> ()
	    with Sys_error err -> print_endline err
		
	  done;
	with End_of_file -> Unix.closedir dh
      end;
      !l
;;
