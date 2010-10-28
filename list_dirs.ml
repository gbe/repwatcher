open Unix;;

let root = "/home/dest/Musique";;

let rec list_dirs dir = 

  let dh_opt = ref None in
    
    begin try
      (* Teste si le répertoire donné en argument existe *)
      match Sys.is_directory dir with
	| true ->  dh_opt := Some (opendir dir)
	| false -> ()
    with Sys_error _ -> ()
    end;
    
      
      match !dh_opt with
	| None -> []
	| Some dh ->
	    let l = ref [dir] in
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
	      !l
;;



let _ =
  let l = list_dirs root in
    Printf.printf "Taille liste: %d\n" (List.length l);
    List.iter (fun d -> print_endline d) l;
    Pervasives.flush Pervasives.stdout
;;
