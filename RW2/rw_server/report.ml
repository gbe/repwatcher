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


open Unix
open Ast
open Ast_conf

let l_reg_char_encoded =

(* &amp; needs to be the first one in the list.
 * Otherwise, &gt; (for example) will be changed in &amp;gt;
 *)
  let l_char = [ ("&", "&amp;")  ;
		 (">", "&gt;")   ;
		 ("<", "&lt;")   ;
		 ("'", "&apos;") ;
		 ((Char.escaped '"'), "&quot;")		 
	       ]
  in
    List.map (fun (char, char_encoded) ->
		((Str.regexp char), char_encoded)
	     ) l_char
;;


let (tor,tow) = Unix.pipe()  ;;	

	
let log (txt, log_level) =

  let conf      = Config.get() in 


  let rec open_fd ?(l=[ O_WRONLY ; O_APPEND]) () = 
    try
      let fd = Unix.openfile "log.txt" l 0o666 in
      
      (* if true then it means the exception was triggered before *)
      if List.mem O_CREAT l then begin
	Unix.fchmod fd 0o666
      end;

      Some fd
      
      
    with Unix_error (err,_,_) ->
      
      match err with
      | ENOENT -> (* no such file or directory *)
	  open_fd ~l:[ O_WRONLY ; O_APPEND ; O_CREAT ] ()
      | _      ->
	  
	  (* Disable logging *)
	  Config.conf := Some {conf with c_log_level = 0};
	  
	  let error = Printf.sprintf "Oops. Couldn't log due to this Unix error: %s. Logging feature disabled" (Unix.error_message err) in
	  prerr_endline error;
	  
	  (* fd = None because it failed to open *)
	  None
  in

  let log_it ()  = 
    
    let to_log = Printf.sprintf "%s\t%s\n" (Date.date()) txt in
    Printf.printf "LOG: %s\n" to_log   ;
    Pervasives.flush Pervasives.stdout;
    (*  ignore (Unix.system ("echo \""^to_log^"\" >> log.txt")) *)

    (* Open the file *)
    match open_fd() with
    | None -> () (* An error occured, the file could not be opened *)
    | Some fd ->
	try
	  ignore (Unix.write fd to_log 0 (String.length to_log));
	  Unix.close fd
	with _ ->
	  prerr_endline "An error occured either trying to log in the file or to close it\n" ;
	  Pervasives.flush Pervasives.stdout
  in
    
  match conf.c_log_level with
  | 0 -> () (* don't log *)
  | 1 ->
      begin
	match log_level with
	| Level_1 -> log_it()
	| Level_2 -> () (* don't log *)
      end
  | 2 -> log_it()
  | _ -> assert false  
	  
;;
    
    
    
let notify txt =

  let conf        = Config.get() in
  
  let txt_escaped =
    List.fold_left (fun txt' (reg, char_encoded) ->
		       Str.global_replace reg char_encoded txt'
		    ) txt l_reg_char_encoded
  in
    
    if conf.c_notify_loc then
      begin
	let call = Printf.sprintf "notify-send -i nobody Repwatcher \"%s\"" txt_escaped in
	  ignore (system call)
      end
    ;
    if conf.c_notify_rem then
      try
	(* Send in the pipe for the server to send to the clients *)
	ignore (Unix.write tow txt_escaped 0 (String.length txt_escaped))
      with _ -> log ("An error occured trying to send in the pipe the notification", Level_1)
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
	    begin	    
	      match Mysqldb.query query with
		| (QueryOK _ | QueryEmpty) -> ()
		| QueryError errmsg        -> log (errmsg, Level_1)
	    end
	      
      | File_Closed ->
	  let id_query = Printf.sprintf "SELECT ID FROM downloads WHERE LOGIN='%s' AND FILENAME='%s' ORDER BY STARTING_DATE DESC LIMIT 1" f.f_login (escape_quote f.f_name)
	  in

	    match Mysqldb.fetch id_query with
	      | QueryEmpty        -> ()
	      | QueryError errmsg -> log (errmsg, Level_1)
	      | QueryOK res       -> 
		  match res with
		    | None -> () (* We do nothing, there is nothing in the database with this login and filename *)
		    | Some ids_array -> 
			
			(* 0 because I know there is only one result returned *)
			match Array.get ids_array 0 with
			  | None -> assert false
			  | Some id ->
			      let query = Printf.sprintf "UPDATE downloads SET ENDING_DATE = '%s' WHERE ID = %s" (Date.date()) id in
				match Mysqldb.query query with
				  | (QueryOK _ | QueryEmpty) -> ()
				  | QueryError errmsg        -> log (errmsg, Level_1)
;;	




module Report =
struct
	(* tor is now viewable from the outside (ssl_server) *)
	let tor = tor ;;

	let report = function
	  | Sql    file_state -> sql file_state
	  | Notify txt        -> notify txt
	  | Log    log'       -> log log'
	;;
end;;
