open Ast;;


(* List the subfolders of a folder.
 * The head of the list returned is the folder given in arg *)
let rec ls folder =
  
  let dh_opt = ref None in
  
  begin try
    (* Check if the given folder exists in argument *)
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
	      (* Test if it's a file or a folder *)
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
	with
	| End_of_file -> Unix.closedir dh
	| Unix.Unix_error (_,function',file_or_dir) ->
	    Report.Report.report (
	    Log (("Unix error: "^function'^", "^file_or_dir), Error) )
      end;
      !l
;;
