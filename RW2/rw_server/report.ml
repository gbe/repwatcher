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
  let to_log = Printf.sprintf "%s\t%s\n" (Date.date()) txt in


  let rec open_fd ?(l=[ O_WRONLY ; O_APPEND]) log_filename = 
    try
      let fd = Unix.openfile log_filename l 0o666 in
      
      (* if true then it means the exception was triggered before *)
      if List.mem O_CREAT l then begin
	Unix.fchmod fd 0o666
      end;
	
	Some fd
	
      
    with Unix_error (err,_,_) ->
      
      match err with
      | ENOENT -> (* no such file or directory *)
	  open_fd ~l:[ O_WRONLY ; O_APPEND ; O_CREAT ] log_filename
      | _      ->
	  
	  (* Disable logging *)
	  Config.conf := Some {conf with c_log_level = Disabled};
	  
	  let error = Printf.sprintf "Oops. Couldn't log due to this Unix error: %s. Logging feature disabled" (Unix.error_message err) in
	    prerr_endline error;
	    
	    (* fd = None because it failed to open *)
	    None
  in


  let log_it ()  = 
    
    let log_filename =
      match log_level with
	  | (Normal  | Normal_Extra) -> "rw.log"
	  | Error                    -> "rw.log.err"
    in

      match open_fd log_filename with
	| None -> prerr_endline "An error occured, the file to log could neither be opened nor created"
	| Some fd ->
	    try
	      ignore (Unix.write fd to_log 0 (String.length to_log));
	      Unix.close fd
		
	    with _ ->
	      prerr_endline "An error occured either trying to log in the file or to close it"
  in

    begin
      (* Only the Errors get printed and on stderr *)
      match log_level with
	| Normal -> ()
	| Normal_Extra -> ()
	| Error                    ->
	    prerr_endline to_log ;
	    Pervasives.flush Pervasives.stdout
    end;


    (* Depending on the user choice, we log *)
    match conf.c_log_level with
      | Disabled -> () (* don't log *)
      | Regular  ->
	  begin
	    match log_level with
	      | Normal       -> log_it ()
	      | Normal_Extra -> () (* don't log *)
	      | Error        -> log_it ()
	  end
      | Debug -> log_it ()
;;
    

let dbus img title txt =   
  let notif_interface = "org.freedesktop.Notifications" in
  let notif_name = notif_interface in
  let notif_path = "/org/freedesktop/Notifications" in

  let send_msg ~bus ~destination ~path ~intf ~serv ~params =
    let msg = DBus.Message.new_method_call destination path intf serv in
      DBus.Message.append msg params;
      let r = DBus.Connection.send_with_reply_and_block bus msg (-1) in
      let l = DBus.Message.get r in
	l
  in
  let send_notif_msg = send_msg ~destination:notif_name ~path:notif_path ~intf:notif_interface in

  let bus = DBus.Bus.get DBus.Bus.Session in
  let params = [
    DBus.String title; (* Name of the program which triggers the notification *)
    DBus.UInt32 1l;
    DBus.String img; (* The picture displayed *)
    DBus.String title;i (* Title of the notification window *)
    DBus.String txt; (* The content *)
    DBus.Array (DBus.Strings []);
    DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigVariant), []));
    DBus.Int32 8000l; (* seems to be the milliseconds the notification must be seen *)
  ] in	
    ignore (send_notif_msg ~bus ~serv:"Notify" ~params)
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
(*	    let call = Printf.sprintf "notify-send -i nobody Repwatcher \"<b>%s</b> %s\n%s\"" login msg_state filename_escaped in
	      ignore (system call)
*)		
	    let dbus_notif = Printf.sprintf "<b>%s</b> %s\n%s" login msg_state filename_escaped in
	      dbus "nobody" "Repwatcher" dbus_notif		
	  end ;

	if conf.c_notify_rem then
	  try
               
	    let str_new_dl = Marshal.to_string ( New_notif (login, filename_escaped, filestate) ) [Marshal.No_sharing] in
	    
	    (* Send in the pipe for the server to send to the clients *)
	    ignore ( Unix.write Pipe.tow str_new_dl 0 (String.length str_new_dl) )
	  with _ -> log ("An error occured trying to send in the pipe the notification", Error)
	      
      end

  | Info_notif info ->
      begin
      	let info_escaped = Txt_operations.escape_for_notify info in
	let conf         = Config.get() in
	if conf.c_notify_loc then
	  begin
(*
	    let call = Printf.sprintf "notify-send -i nobody Repwatcher \"%s\"" info_escaped in
	    ignore (system call)
*)
	    dbus "nobody" "Repwatcher" info_escaped
	  end ;

	if conf.c_notify_rem then
	  try
            
	    let str_info = Marshal.to_string ( Info_notif info_escaped ) [Marshal.No_sharing] in
	    
	    (* Send in the pipe for the server to send to the clients *)
	    ignore ( Unix.write Pipe.tow str_info 0 (String.length str_info) )
	  with _ -> log ("An error occured trying to send in the pipe the notification", Error)
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
	    (* Connect to Mysql *)
	    match Mysqldb.connect() with
	    | Some error -> log (error, Error)
	    | None       ->

		log ("Connected to MySQL", Normal_Extra);
		log (("Next SQL query to compute:\n"^query^"\n"), Normal_Extra);

		(* Do the query *)
		(match Mysqldb.query query with
		| (QueryOK _ | QueryEmpty) -> log ("Query successfully executed", Normal_Extra)
		| QueryError error         -> log (error, Error)
		);
	
		(* Disconnect *)
		match Mysqldb.disconnect() with
		| Some error -> log (error, Error)
		| None       -> log ("Disconnected from MySQL", Normal_Extra);
	  end

      | File_Closed ->
	  let id_query = Printf.sprintf "SELECT ID FROM downloads WHERE LOGIN=%s AND FILENAME=%s ORDER BY STARTING_DATE DESC LIMIT 1"
	      (Mysqldb.ml2str f.f_login)
	      (Mysqldb.ml2str f.f_name)
	  in

	  (* Connect to Mysql *)
	  match Mysqldb.connect() with
	  | Some error -> log (error, Error)
	  | None ->

	      log ("Connected to MySQL", Normal_Extra);
	      log (("Next SQL query to compute:\n"^id_query^"\n"), Normal_Extra);
	      
	      begin
		(* Do the query *)
		match Mysqldb.fetch id_query with
		| QueryEmpty        -> log ("Query successfully executed but no result returned", Normal_Extra)
		| QueryError errmsg -> log (errmsg, Error)
		| QueryOK res       ->

		    log ("Query successfully executed", Normal_Extra);

		    match res with
		    | None ->
			log ( ("Error. Previous query successfully executed but nothing was returned. This means something is wrong. Please check and try in your database the following query :\n"^id_query), Error );

		    | Some ids_array -> 
			
			(* 0 because I know there is only one result returned *)
			match Array.get ids_array 0 with
			| None -> assert false
			| Some id ->
			    let query = Printf.sprintf "UPDATE downloads SET ENDING_DATE = %s WHERE ID = %s"
				(Mysqldb.ml2str (Date.date()))
				(Mysqldb.ml2str id) in

			    log (("Next SQL query to compute:\n"^query^"\n"), Normal_Extra);

			    match Mysqldb.query query with
			    | QueryOK _          -> log ("Query successfully executed and returned a result", Normal_Extra)
			    | QueryEmpty         -> log ("Query successfully executed but no result returned", Normal_Extra)
			    | QueryError errmsg  -> log (errmsg, Error)
	      end;

	      (* Disconnect *)
	      match Mysqldb.disconnect() with
	      | Some error -> log (error, Error)
	      | None       -> log ("Disconnected from MySQL", Normal_Extra)
;;




module Report =
struct
  
  let report = function
    | Sql    (file, state)    -> sql (file, state)
    | Notify notification     -> notify notification
    | Log    (txt, log_level) -> log (txt, log_level)
;;
end;;
