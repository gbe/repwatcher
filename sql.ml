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
open Mysql
open Unix

let dateML date =
  (* Starts in 1900 *)
  let year = string_of_int (1900 + date.tm_year) in
    (* month = 0..11 *)
  let month = string_of_int (1 + date.tm_mon) in
  let day = string_of_int date.tm_mday in
  let hour = string_of_int date.tm_hour in
  let minute = string_of_int date.tm_min in
  let second = string_of_int date.tm_sec in
  
  (* 2008-01-19 16:21:00 *)
  year^"-"^month^"-"^day^" "^hour^":"^minute^":"^second





let get_date_and_cid c_sql =
  
  (* connexion id *)
  let cid = Mysql.connect c_sql in
    (dateML (localtime(time())), cid)





let escape_apostrophe s =  
  let r = Str.regexp "'" in
  Str.global_replace r "''" s
    


let action_insert_table cid f date =
  
  let str_filesize = Int64.to_string f.f_filesize in
    
  let query = "insert into downloads (login,program,path,filename,filesize,starting_date) values
('"^f.f_login^"', '"^f.f_prog_source^"', '"^f.f_path^"', '"^(escape_apostrophe f.f_name)^"', '"^str_filesize^"', '"^date^"')" in

  let _ = exec cid query in ()





(* Lance la fonction d'insertion si le fichier :
   - n'existe pas dans la table
   - existe mais la derniere insertion date de plus de 6h
*)
let check_insert_table cid f date =

  (* execution de la requete, et recuperation du resultat *)
  let res =
    Mysql.fetch (Mysql.exec cid ("SELECT ID FROM downloads WHERE LOGIN='"^f.f_login^"' AND FILENAME='"^(escape_apostrophe f.f_name)^"'"))
  in
  (* on regarde si la requete renvoie quelque chose *)
  match res with
      None       ->  (* Le fichier n'existe pas, j'insert *) action_insert_table cid f date
    | Some stuff ->
	(* pour chaque champ, s’il existe, on l’affiche
	 * le None de caml equivaut ici au NULL de MySQL *)
	Array.iter (
          function
              None     -> ()
            | Some str ->
		let result = Mysql.fetch (Mysql.exec cid ("SELECT ID FROM downloads WHERE ID=
(SELECT ID FROM downloads WHERE LOGIN='"^f.f_login^"' AND FILENAME='"^(escape_apostrophe f.f_name)^"' ORDER BY STARTING_DATE DESC LIMIT 1)
 AND TIMEDIFF('"^date^"', STARTING_DATE)>='06:00:00'"))
		in
		match result with
		    None       -> (* Le fichier existe mais ca fait moins de 6h *) ()
		  | Some stuff ->
		      (* pour chaque champ, s’il existe, on l’affiche
		       * le None de caml equivaut ici au NULL de MySQL *)
		      Array.iter (
			function
			    None     -> ()
			  | Some str -> (* Le fichier existe mais ca fait plus de 6h, j'insert *) action_insert_table cid f date
			      
		      )
			stuff
	)
	  stuff



let insert p c_sql =
  
  let (date,cid) = get_date_and_cid c_sql
  in
    List.iter (fun f -> check_insert_table cid f date) p;
    Mysql.disconnect cid
      



      
let action_update file_finished cid date id =
  let query = "UPDATE downloads SET ENDING_DATE='"^date^"' WHERE ID = "^id
  in
  let _ = exec cid query in ()
			      
  




let check_update file_finished cid date =
  
  let res = Mysql.fetch (Mysql.exec cid ("SELECT ID FROM downloads WHERE LOGIN='"^file_finished.f_login^"' AND FILENAME='"^(escape_apostrophe file_finished.f_name)^"' ORDER BY STARTING_DATE DESC LIMIT 1"))
  in
    
    match res with
	None -> assert false (* cas impossible car pour updater, le fichier existe forcément déjà, sauf si le fichier a été supprimé de la table à la main *)
      | Some tableau_ids ->
	  
	  (* Avec notre SELECT, on ne récupère qu'une seule valeur d'où le get 0 *)
	  let str_option_id = Array.get tableau_ids 0 in
	    
	    match str_option_id with
		None -> assert false
	      | Some id -> action_update file_finished cid date id
		  


	
let update_ending_date files_finished c_sql =
  let (date,cid) = get_date_and_cid c_sql in
    List.iter (fun f -> check_update f cid date) files_finished;
    Mysql.disconnect cid
      
