(* TO DO

- Ajouter le SQL
- Modifier le fichier de config pour prendre en compte de nouveaux paramètres
- Voir pour mettre en surveillance des fichiers et pas seulement des dossiers (cf fichier de config donc)
- Dans report.ml, revoir le système des erreurs. Mettre des numéros d'erreur et appeler directement dans Report les erreurs. Enlever les add_report depuis Core.ml
- Ajouter deux options : pour notifier localement et/ou activer le serveur
- Déplacer l'initalisation de la configuration afin de pouvoir la récupérer dans report et ajouter un test pour les notifications
*)



open Unix
open Printf

open Ast
open Ast_conf
open Core
open Report


let config_file = "conf/repwatcher.conf"    (* Nom du fichier de configuration *)



(* Check if the config exists and then
 * - Parse it
 * - Put a watch on it
 *
 * Then watch the directories
 *)
let init () =

  let load_and_watch_config () =

      if Sys.file_exists config_file then
	begin
	  (* Get the configuration file *)
	  let conf = Config.parse config_file in
	  (* Watch it *)
	  Core.add_watch config_file None true;
	  conf
	end
      else
	failwith "Config file doesn't exist"
    in

  (* Set the watch on the directories *) 
  let watch_dirs directories =   
    
    let ldirs = List.map (
      fun dir ->
	(* If the directory name ends with a '/' then it is deleted *)
	if String.get dir ((String.length dir) -1) = '/' then
	  String.sub dir 0 ((String.length dir)-1)
	else dir)
      directories
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
	    prerr_endline error ;
	    Report.report (Log error)
	  end
	(* No such file or directory *)
	with Sys_error e -> let error = "Error: "^e in 
			       prerr_endline error ;
			       Report.report (Log error)
    ) ldirs 
  in
  
  let conf = load_and_watch_config () in
  watch_dirs conf.c_directories ;
  conf



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
  let conf = init () in

  let fd = match conf.c_notify_rem with
  | true  -> Report.report (Log "Start server for remote notifications") ; Unix.fork()
  | false -> -1
  in


  match fd with
    | 0 -> if conf.c_notify_rem then Ssl_server.run Report.tor
    | _ ->
	begin
	      
	    Core.print_ht ();
	    
	    Pervasives.flush Pervasives.stdout;
	    
	    Report.report (Notify "Repwatcher is watching youuu ! :)") ;
	    Report.report (Log "Repwatcher is watching youuu ! :)") ;	    
     
	      while true do
		
		let _,_,_ = Unix.select [ Core.fd ] [] [] (-1.) in
		let event_l = Inotify.read Core.fd in
		  
		  List.iter (fun event -> Core.what_to_do event) event_l;
	      done;
	      
	      Unix.close Core.fd
	end
;;
