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


open Ast
open Ast_conf


let crea_l_fichiers channel =
  
  let l = ref [] in    
  let exp_decoup_espace= Str.regexp "[' ']+" in
  let fichier = {f_name="";f_path="";f_login="";f_filesize=(Int64.of_int 0);f_prog_source=""} in
    
    begin
      try
	while true do 
	  let ligne = input_line channel in
	    
	  let ligne_decoupee = Str.split exp_decoup_espace ligne in
	    
	  let rec crea_enregistrement_fichier ligne_decoupee colonne fichier =
	    
	    match ligne_decoupee with
		[] -> fichier
	      | t::q -> if colonne = 0 then
		  crea_enregistrement_fichier q (colonne+1) {fichier with f_prog_source = t}
		    
		else if colonne = 1 || colonne = 3 || colonne = 4 || colonne = 5 || colonne = 7 then
		  crea_enregistrement_fichier q (colonne+1) fichier
		    
		(* pseudo *)
		else if colonne = 2 then
		  crea_enregistrement_fichier q (colonne+1) {fichier with f_login = t}
		    
		(* taille fichier *)
		else if colonne = 6 then
		  crea_enregistrement_fichier q (colonne+1) {fichier with f_filesize = (Int64.of_string t)}
		    
		else
		  let path_and_filename = t^" "^(String.concat " " q)
		  in {fichier with f_name = (Filename.basename path_and_filename) ; f_path = (Filename.dirname path_and_filename)^"/"}		    
		       
	  in l := (crea_enregistrement_fichier ligne_decoupee 0 fichier)::(!l)
	done
      with _ -> close_in channel
    end;
    !l
      







(* Retourne la liste de tous les fichiers parsés depuis tous les répertoires *)
let ret_l_files conf ifile file_specified =
  
  
  (* Retourne une liste de tous les fichiers ouverts dans les dossiers surveillés *)
  let unfiltred_l =
    
    (* The input file has been specified so as to parse it later *)
    if file_specified = true then
      
      (* Open a channel with the input file *)
      let in_chan = open_in ifile in
	(* Create the ast.file list *)	
      let l_opened_files = crea_l_fichiers in_chan in
	(* Close the file *)
      let _ = close_in in_chan in
	
	(* **************************** *)
	(* Return the ast.file list     *)
	l_opened_files
        (* **************************** *)
	  
	  
    (* File not specified, we do some system calls and then lexing-parsing *)
    else
      begin
	let in_chan = Unix.open_process_in "(lsof -u ^root 2> /dev/null)| grep REG" in
	let l_opened_files = crea_l_fichiers in_chan in
	  (* Ferme le channel du process lsof *)
	let _ = Unix.close_process_in in_chan in
	  
	  (* **************************** *)
	  (* Return the opened files list *)	  
	  l_opened_files
	  (* **************************** *)	 
      end
  in


   
    (* Check wether we're in specified mode or unwanted mode  *)      
    (* Remove all the files which the program is not in the specified_programs list *)
    if conf.c_mode = "specified_programs" then

      if List.length conf.c_specified_programs > 0 then
	let l_prog_filtered = List.filter (fun file -> List.mem file.f_prog_source conf.c_specified_programs) unfiltred_l in

	  (* Filtre les répertoires et retourne la liste des fichiers à insérer *)
	  List.filter (fun file ->
			 (* renvoie un booléen pour dire si le path du fichier est l'un des répertoires surveillés *)
			 List.exists (fun directory ->
					let rexp = Str.regexp directory in
					  Str.string_match rexp file.f_path 0
				     ) conf.c_directories			   
		      ) l_prog_filtered	(* l_user_filtered remplacé par l_prog_filtered *)		    			   
	    
      else
	begin
	  print_endline "Error. Your 'specified_programs' list is empty. Exiting...";
	  exit 1
	end
	  
	  
	  
    (* Remove all the unwanted_programs from the list *)
    else if conf.c_mode = "unwanted_programs" then
      
      begin
	(* Enlève les programmes que l'on ne veut pas *)
	let l_prog_filtered = List.filter (fun file -> if List.mem file.f_prog_source conf.c_unwanted_programs then false else true) unfiltred_l in

	  (* Filtre les répertoires et retourne la liste des fichiers à insérer *)
	  List.filter (fun file ->
			 (* renvoie un booléen pour dire si le path du fichier est l'un des répertoires surveillés *)
			 List.exists (fun directory ->
					let rexp = Str.regexp directory in
					  Str.string_match rexp file.f_path 0
				     ) conf.c_directories
		      ) l_prog_filtered	
      end
	
    (* Normally it's an impossible case *)
    else
      assert false
	
(* Fin du l_files *)
















