(*
    Repwatcher
    Copyright (C) 2009-2011  Gregory Bellier

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


open Types
open Types_conf


   
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

  let send_notif_msg =
    send_msg ~destination:notif_name ~path:notif_path ~intf:notif_interface
  in

  let bus = DBus.Bus.get DBus.Bus.Session in
  let params = [
    DBus.String title; (* Name of the program which triggers the notification *)
    DBus.UInt32 1l;
    DBus.String img; (* The picture displayed *)
    DBus.String title; (* Title of the notification window *)
    DBus.String txt; (* The content *)
    DBus.Array (DBus.Strings []);
    DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigVariant), []));
    DBus.Int32 15000l; (* milliseconds the notification must be seen *)
  ] in	
    ignore (send_notif_msg ~bus ~serv:"Notify" ~params)
;;


    
    
let notify notification =  
  
  match notification with
  | New_notif (file, filestate) ->
      begin

	let filename_escaped = Txt_operations.escape_for_notify file.f_name in
	let conf             = Config.get() in  

	if conf.c_notify.n_locally then
	  begin
	    let msg_state =
	      match filestate with
	      | File_Opened -> "is downloading"
	      | File_Closed -> "finished downloading"
	    in

	    (* Return the n last folders *)
	    let rec n_last_elements l n =
	      match l with
	      | [] -> "./"
	      | [t] -> t^"/"
	      | t::q ->
		  if List.length q < n then
		    t^"/"^(n_last_elements q n)
		  else
		    n_last_elements q n
	    in

(*	    let call = Printf.sprintf "notify-send -i nobody Repwatcher \"<b>%s</b> %s\n%s\"" login msg_state filename_escaped in
	      ignore (system call)
*)	

	    
	    let r = Str.regexp "/" in
	    let l_folders = Str.split r file.f_path in

	    
	    let dbus_notif =
	      match conf.c_notify.n_parent_folders with
	      | None -> Printf.sprintf "<b>%s</b> %s\n%s" file.f_login msg_state filename_escaped 
	      | Some parent_folders ->
		  Printf.sprintf "<b>%s</b> %s\n%s%s"
		    file.f_login msg_state (n_last_elements l_folders parent_folders) filename_escaped
	    in
	    dbus "nobody" "Repwatcher" dbus_notif

	  end ;

	if conf.c_notify.n_remotely.r_activate then
	  try
               
	    let str_new_dl =
	      Marshal.to_string 
		(New_notif ({file with f_name = filename_escaped}, filestate) )
		[Marshal.No_sharing]
	    in
	    
	    (* Send in the pipe for the server to send to the clients *)
	    ignore ( Unix.write Pipe.tow str_new_dl 0 (String.length str_new_dl) )
	  with _ ->
	    Log.log ("An error occured trying to send in the pipe the notification", Error)
	      
      end

  | Local_notif info ->
      let info_escaped = Txt_operations.escape_for_notify info in
      let conf         = Config.get() in
      if conf.c_notify.n_locally then
	begin
(*
  let call = Printf.sprintf "notify-send -i nobody Repwatcher \"%s\"" info_escaped in
  ignore (system call)
 *)
	  dbus "nobody" "Repwatcher" info_escaped
	end


  | Old_notif l_current_dls ->
      (* Txt_operations.escape_for_notify has already been done on f_name *)
      let str_current_dls = Marshal.to_string (Old_notif l_current_dls) [Marshal.No_sharing] in

      (* send though the pipe to be processed by the server *)
      ignore (Unix.write Pipe.tow str_current_dls 0 (String.length str_current_dls))
;;




let sql (f, state, date) =
  
  match state with
  | File_Opened  ->	  
      
      (* ml2str adds quotes. ml2str "txt" -> "'txt'" *)
      let query =
	Printf.sprintf "INSERT INTO downloads \
	  (login,program,path,filename,filesize,starting_date, in_progress) \
	  VALUES (%s, %s, %s, %s, %s, %s, '1')"
          (Mysqldb.ml2str f.f_login)
	  (Mysqldb.ml2str f.f_prog_source)
	  (Mysqldb.ml2str f.f_path)
	  (Mysqldb.ml2str f.f_name)
	  (Mysqldb.ml2str (Int64.to_string f.f_filesize))
	  (Mysqldb.ml2str date)
      in
         
      ignore (Mysqldb.query query)
	
	
	
  | File_Closed ->

      let update_query =
	Printf.sprintf "UPDATE downloads \
	  SET ENDING_DATE = %s, IN_PROGRESS = '0' \
	  WHERE LOGIN=%s AND \
	  FILENAME=%s AND \
	  IN_PROGRESS = 1 \
	  ORDER BY STARTING_DATE DESC \
	  LIMIT 1"
	  (Mysqldb.ml2str date)
	  (Mysqldb.ml2str f.f_login)
	  (Mysqldb.ml2str f.f_name)
      in

      ignore (Mysqldb.query update_query)
;;




module Report =
struct
  
  let report = function
    | Sql (file, state, date) ->
	sql (file, state, date)

    | Notify notification ->
	notify notification
;;
end;;
