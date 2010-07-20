(*
    Repwatcher
    Copyright (C) 2009  Gregory Bellier

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


open Unix
open Ast
open Ast_conf

let log (txt, log_level) =

  let conf = Config.get() in 


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
    Printf.printf "LOG: %s" to_log   ;
    Pervasives.flush Pervasives.stdout;

    match open_fd() with
    | None -> prerr_endline "An error occured, the file to log could neither be opened nor created"
    | Some fd ->
	try
	  ignore (Unix.write fd to_log 0 (String.length to_log));
	  Unix.close fd

	with _ ->
	  prerr_endline "An error occured either trying to log in the file or to close it"
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
    
    
    
let notify notification =  
  
  match notification with
  | New_notif (login, filename, filestate) ->
      begin

	let filename_escaped = Txt_operations.escape_for_notify filename in
	let conf             = Config.get() in  

	if conf.c_notify_loc then
	  begin
	    let msg_state =
	      match filestate with
	      | File_Opened -> "is downloading"
	      | File_Closed -> "finished downloading"
	    in
	    let call = Printf.sprintf "notify-send -i nobody Repwatcher \"<b>%s</b> %s\n%s\"" login msg_state filename_escaped in
	    ignore (system call)
	  end ;

	if conf.c_notify_rem then
	  try
               
	    let str_new_dl = Marshal.to_string ( New_notif (login, filename_escaped, filestate) ) [Marshal.No_sharing] in
	    
	    (* Send in the pipe for the server to send to the clients *)
	    ignore ( Unix.write Pipe.tow str_new_dl 0 (String.length str_new_dl) )
	  with _ -> log ("An error occured trying to send in the pipe the notification", Level_1)
	      
      end

  | Info_notif info ->
      begin
      	let info_escaped = Txt_operations.escape_for_notify info in
	let conf         = Config.get() in
	if conf.c_notify_loc then
	  begin
	    let call = Printf.sprintf "notify-send -i nobody Repwatcher \"%s\"" info_escaped in
	    ignore (system call)
	  end ;

	if conf.c_notify_rem then
	  try
            
	    let str_info = Marshal.to_string ( Info_notif info_escaped ) [Marshal.No_sharing] in
	    
	    (* Send in the pipe for the server to send to the clients *)
	    ignore ( Unix.write Pipe.tow str_info 0 (String.length str_info) )
	  with _ -> log ("An error occured trying to send in the pipe the notification", Level_1)
      end


  | _ -> assert false (* Old_notif is not allowed here *)
;;




let sql (f, state) =
    
    match state with
      | File_Opened  ->
	  let query = Printf.sprintf "INSERT INTO downloads (login,program,path,filename,filesize,starting_date) VALUES (%s, %s, %s, %s, %s, %s)"
            (Mysqldb.ml2str f.f_login)
	    (Mysqldb.ml2str f.f_prog_source)
	    (Mysqldb.ml2str f.f_path)
	    (Mysqldb.ml2str f.f_name)
	    (Mysqldb.ml2str (Int64.to_string f.f_filesize))
	    (Mysqldb.ml2str (Date.date()))
	  in
	    begin	    
	      match Mysqldb.query query with
		| (QueryOK _ | QueryEmpty) -> ()
		| QueryError errmsg        -> log (errmsg, Level_1)
	    end
	      
      | File_Closed ->
	  let id_query = Printf.sprintf "SELECT ID FROM downloads WHERE LOGIN=%s AND FILENAME=%s ORDER BY STARTING_DATE DESC LIMIT 1"
	    (Mysqldb.ml2str f.f_login)
	    (Mysqldb.ml2str f.f_name)
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
			      let query = Printf.sprintf "UPDATE downloads SET ENDING_DATE = %s WHERE ID = %s"
				(Mysqldb.ml2str (Date.date()))
				(Mysqldb.ml2str id) in
				match Mysqldb.query query with
				  | (QueryOK _ | QueryEmpty) -> ()
				  | QueryError errmsg        -> log (errmsg, Level_1)
;;	




module Report =
struct
  
  let report = function
    | Sql    (file, state)   -> sql (file, state)
    | Notify notification    -> notify notification
    | Log    log'            -> log log'
;;
end;;
