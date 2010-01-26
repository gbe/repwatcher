(* TO DO

- Ajouter le SQL
- Modifier le fichier de config pour prendre en compte de nouveaux paramètres
- Voir pour mettre en surveillance des fichiers et pas seulement des dossiers (cf fichier de config donc)
- Revoir tout ce qui concerne le init et reinit

*)



open Unix
open Printf

open Ast
open Ast_conf


let conf = ref {
  c_directories = [] ;
  c_mode = Unwanted_programs ;
  c_specified_programs = [] ; 
  c_unwanted_programs = [] ;
  c_sql = {
    dbhost = None ;
    dbname = None ;
    dbport = None ;
    dbpwd  = None ;
    dbuser = None ;
  }
}

let config_file = "conf/repwatcher.conf"    (* Nom du fichier de configuration *)



(* Check if the config exists and then
 * - Parse it
 * - Put a watch on it
 *
 * Then watch the directories
 *)
let init () =

  let load_config () =
    if Sys.file_exists config_file then
      begin
	(* Get the configuration file *)
	conf := Configuration.parse_config config_file;
	Core.add_watch config_file None true
      end
  in

  (* Set the watch on the directories *) 
  let set_watches () =   
    
    let dirs = List.map (
      fun dir ->
	(* If the directory name ends with a '/' then it is deleted *)
	if String.get dir ((String.length dir) -1) = '/' then
	  String.sub dir 0 ((String.length dir)-1)
	else dir)
      !conf.c_directories
    in
      
    List.iter (
      fun dir ->
	try
	  if Sys.is_directory dir then
	    let _ = Core.add_watch dir None false in
	    let l = Core.ls_children dir in
	      Core.add_watch_children l
	else (* is not *)
	  begin
	    let error = "Error '"^dir^"' is NOT a directory" in
	    prerr_endline error(*;
	    log error*)
	  end
	with Sys_error e -> (let error = "Error: "^e in 
			       prerr_endline error(*;
			       log error*)
			    )
    ) dirs 
  in
    load_config ();
    set_watches ()




(* Fonction main *)
let _ =
  
  (* Verifie que le programme est lance en root seulement *)
(*  if (Unix.getuid()) <> 0 then
    begin
      print_endline "This program has to be executed as root\n";
      exit 1
    end;
*)


  (* Load the configuration file and then watch the directories given in it *)
  (* init (); *)


  match Unix.fork() with
    | 0 -> Ssl_server.run Go.tor
    | _ ->
	begin
	      
	    Core.print_ht ();
	    
	    Pervasives.flush Pervasives.stdout;
	    
	    Go.notify "Repwatcher is watching youuu ! :)";
	    Go.log "Repwatcher is watching youuu ! :)";	    
     
	      while true do
		
		let _,_,_ = Unix.select [ Core.fd ] [] [] (-1.) in
		let event_l = Inotify.read Core.fd in
		  
		  List.iter (fun event ->
			       
			       (* let (_,tel,_,_) = event in
				  
				  print_endline ("\n-------");
				  List.iter (fun type_event -> print_endline (string_of_event type_event) ) tel;
				  print_endline ("---------\n");
			       *)
			       
			       let _ = Core.what_to_do event conf in
		
				 printf "Hashtable :%d\n" (Hashtbl.length Core.ht_iwatched);
				 Pervasives.flush Pervasives.stdout
			    ) event_l;
	      done;
	      
	      Unix.close Core.fd
	end