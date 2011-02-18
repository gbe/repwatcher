(*
    This file is part of Repwatcher.

    Repwatcher is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Repwatcher is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Repwatcher; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*)





(* Fichier principal de Repwatcher *)
open Format
open Ast
open Ast_conf
  
let num_version = "0.4.1"
let verbose = ref false                (* Option d'exécution, si vous voulez écrire dans le terminal tout ce qui s'ajoute dans la base *)
let file_specified = ref false
let ifile = ref ""                     (* Nom du fichier source *)
let config_file = "repwatcher.conf"    (* Nom du fichier de configuration *)

let last_list_inserted_in_db = ref []  (* On conserve la liste des derniers elts ajoutes en DB *)
                                       (* pour pouvoir determiner la date de fin du telechargement d'un fichier *)
                                       (* par comparaison avec la nouvelle liste generee *)

let timer_sleep_loop = ref 15          (* Default time between 2 checks *)


let set_file f s = f :=s

let usage = "usage:
\trepwatcher [option] to start the program
\tor
\trepwatcher [option] file.txt to insert the data contained in file.txt into the database\n"


(* Les options de Repwatcher que l'on affiche en tapant repwatcher --help *)

let options = 
  [("-verbose",Arg.Set verbose , 
    "  To print a lot of stuff")]
    
  

let update_db conf l_files_currently_downloaded =
  
  (* Si le fichier a été spécifié, alors on dit modifie la BD pour modifier la date de fin de téléchargement de NULL *) 
  if !file_specified then
    let _ = Sql.update_ending_date l_files_currently_downloaded conf.c_sql in () 
  else
    
    begin 
      (* Cree une liste de tous les fichiers qui ne sont plus en cours de telechargement *)
      let l_to_update = List.filter (fun file -> if List.mem file l_files_currently_downloaded then false else true ) !last_list_inserted_in_db in

      let _ = Sql.update_ending_date l_to_update conf.c_sql in
	
	(if !verbose then
	   List.iter (fun f -> print_endline (f.f_login^" has finished to download the file "^f.f_name^"\n")) l_to_update
	);
	
	(* Sert pour mettre à jour la date de fin des téléchargements *)
	last_list_inserted_in_db := l_files_currently_downloaded
    end



	
(* We get all the files to insert into the database *)	  
let get_l_files_and_insert_db conf =
  
  let l_files = (Gen_liste_fichiers.ret_l_files conf !ifile !file_specified) in
    
    if !verbose then
      begin
	List.iter (fun f ->
		     print_endline ("Fichier: "^f.f_name);
		     print_endline ("Path: "^f.f_path);
		     print_endline ("Prog: "^f.f_prog_source);
		     print_endline ("Taille: "^(Int64.to_string f.f_filesize));
		     print_endline ("Login: "^f.f_login^"\n")
		  ) l_files;
	Pervasives.flush stdout;
      end;
    
    
    (* Here is where we insert the files into the database *)
    let _ = Sql.insert l_files conf.c_sql in

      (* Return the list of the files currently downloaded *)
      l_files

      
      
     






(* fonction main *)
let _ = 
  
  (* Verifie que le programme est lance en root seulement *)
  if (Unix.getuid()) <> 0 then
    begin
      print_endline "This program has to be executed as root\n";
      exit 1
    end;
  
  
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;
  
  
  (* Verifie si un nom de fichier est donné *)
  (if !ifile="" then
     file_specified := false
   else
     begin
       file_specified := true;
       
       (* The file has to have the .txt suffix *)
       if not (Filename.check_suffix !ifile ".txt") then 
	 (eprintf "Le fichier d'entree doit avoir l'extension .txt\n@?";
	  Arg.usage options usage;exit 1)
     end
  );
  

  

  
  (* Récupère le fichier de configuration *)
  let conf = Configuration.parse_config config_file in
  (* Fin de la recuperation du fichier de config *)
  
  
  print_endline "################################################";
  print_endline ("Repwatcher v"^num_version^" is running in "^conf.c_mode^" mode");

  (* Set the timer of the loop with the data from the configuration file. If not set then it's the default value *)
  timer_sleep_loop := 
    begin
      match conf.c_sleep_loop with
	  Some x -> 
	    (* Print only if we set verbose and without a demo file in repwatcher's argument *)
	    if !verbose && not !file_specified then
	      begin
		print_endline ("Timer sleep loop : "^(string_of_int x)^" seconds")
	      end ;
	    x
	| None -> !timer_sleep_loop
    end
  ;
  print_endline "\n################################################";

  Pervasives.flush stdout;
  

  (* Si le fichier a été donné sur la ligne de commande *)
  if !file_specified = true then

    begin     
      (* On insère la liste dans la base de données *)
      let l_files = get_l_files_and_insert_db conf in
	update_db conf l_files;
      
      (* Puis on quitte le programme *)
      exit 0
    end
      




  (* Le fichier n'a pas été donné, le programme va tourner en boucle *)
  else
    
    (* required for the specified mode *)
    let num_process = ref 0 in
    
   
    (* The main goes on here *)
    while true do
      
      if conf.c_mode = "specified_programs" then
	begin
	  
	  (* Do the unix command for each specified program *)
	  List.iter (fun program -> 
		       let in_chan = Unix.open_process_in ("ps -e | grep "^program^" | wc -l") in
		       
			 (if program = "proftpd" then
			    num_process := !num_process -1);
			 num_process := !num_process + int_of_string (input_line in_chan)
			 ;

			 let _ = Unix.close_process_in in_chan in ()
		    ) conf.c_specified_programs;
	  
	  (* Flush stdout to print right now *)
	  Pervasives.flush stdout;
	  
	  
	  (* if at least one process we're listening to is running *)
	  (if !num_process >= 1 then
	    (* insert into the database the files currently downloaded *)
	    let l_files_currently_dled = get_l_files_and_insert_db conf in
	      (* and then update the DB to update the ending_date *)
	      update_db conf l_files_currently_dled

	  else
	    (* update the ending_date of the files downloaded *)
	    (* in the arguments, the list is empty because there are no downloading at this time *)
	    update_db conf []
	  );
	  
	  
	  (* Reset the processes number for the next loop *)
	  num_process := 0
	  
	end (* End of the specified mode *)
	  
	

	  
      (* If the program is running with unwanted mode *)  
      else(
	
	let l_files = get_l_files_and_insert_db conf in
	  update_db conf l_files
      );
      
      (* Sleep during timer_sleep_loop seconds, set in the configuration file. If not then it's the default value *)
      Unix.sleep !timer_sleep_loop
	
    done
      
