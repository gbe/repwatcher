open Unix;;

let root = "/home/dest/Musique";;

let rec list_dirs dir = 

  let l = ref [] in
  let dh_opt = ref None in


    (try
       (* Teste si le répertoire donné en argument existe *)
       match Sys.is_directory dir with
	 | true ->  dh_opt := Some (opendir dir)
	 | false -> ()
     with Sys_error _ -> ()
    );
    
    
    match !dh_opt with
      | None -> []
      | Some dh ->
	  begin
	    try
	      while true do
		let entry = readdir dh in
		  try
		    let fullpath = (dir^"/"^entry) in
		    (* Teste si c'est un fichier ou un dossier *)
		    match Sys.is_directory fullpath with
		      | true ->
			  begin
			    match entry with
			      | ("." | "..") -> ()
			      | _            -> l := (!l)@(list_dirs fullpath)
			  end
		      | false -> ()
		  with Sys_error err -> print_endline err
		    
	      done;
	    with End_of_file -> closedir dh
	  end;
	  dir::(!l)
;;



let _ =
  let l = list_dirs root in
  List.iter (fun d -> print_endline d) l;
  Pervasives.flush Pervasives.stdout
;;
