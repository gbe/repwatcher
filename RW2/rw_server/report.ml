open Unix
open Ast
open Ast_conf

let l_reg_char_encoded =

(* &amp; needs to be the last one of the list.
 * Otherwise, &gt; (for example) will be changed in &amp;gt;
 *)
  let l_char = [ (">", "&gt;") ;
		 ("<", "&lt;") ;
		 ("'", "&apos;") ;
		 ((Char.escaped '"'), "&quot;");
		 ("&", "&amp;")
	       ]
  in
  List.map (fun (char, char_encoded) ->
    ((Str.regexp char), char_encoded)
	   ) l_char
;;


let (tor,tow) = Unix.pipe();;	
    	
let log txt =
  let to_log = Printf.sprintf "%s\t%s" (Date.date()) txt in
  Printf.printf "LOG: %s\n" to_log   ;
  Pervasives.flush Pervasives.stdout ;
  ignore (Unix.system ("echo \""^to_log^"\" >> log.txt"))
;;
    
    
    
let notify txt =
  Printf.printf "Notify: %s\n" txt;  
  
  let conf = Config.get() in

  let txt_escaped =
    List.fold_right (fun (reg, char_encoded) txt' ->
      Str.global_replace reg char_encoded txt'
		 ) l_reg_char_encoded txt
  in

  if conf.c_notify_loc then
    begin
      let call = Printf.sprintf "notify-send -i nobody Repwatcher \"%s\"" txt_escaped in
      ignore (system call)
    end
      ;
  if conf.c_notify_rem then
    (* Send in the pipe for the server to send to the clients *)
    ignore (Unix.write tow txt_escaped 0 (String.length txt_escaped))
;;




let sql (f, state) =
  
  let escape_quote s =  
    let r = Str.regexp "'" in
      Str.global_replace r "''" s
  in
    
    match state with
      | File_Opened  ->
	  let query = Printf.sprintf "INSERT INTO downloads (login,program,path,filename,filesize,starting_date) VALUES ('%s', '%s', '%s', '%s', '%s', '%s')"
            f.f_login f.f_prog_source f.f_path (escape_quote f.f_name) (Int64.to_string f.f_filesize) (Date.date())
	  in
	    ignore (Mysqldb.query query)
	      
      | File_Closed ->
	  let id_query = Printf.sprintf "SELECT ID FROM downloads WHERE LOGIN='%s' AND FILENAME='%s' ORDER BY STARTING_DATE DESC LIMIT 1" f.f_login (escape_quote f.f_name)
	  in

	  let res = Mysqldb.fetch id_query in
	    match res with
	      | None -> () (* We do nothing, there is nothing in the database with this login and filename *)
	      | Some ids_array -> 

		  (* 0 because I know there is only one result returned *)
		  match Array.get ids_array 0 with
		    | None -> assert false
		    | Some id ->
			let query = Printf.sprintf "UPDATE downloads SET ENDING_DATE = '%s' WHERE ID = %s" (Date.date()) id in
			  ignore (Mysqldb.query query)
;;	




module Report =
struct
	(* tor is now viewable from the outside (ssl_server) *)
	let tor = tor ;;

	let report = function
	  | Sql    file_state -> sql file_state
	  | Notify txt        -> notify txt
	  | Log    txt        -> log txt
	;;
end;;
