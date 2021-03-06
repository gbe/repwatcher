open Types
open Types_conf
open Sql_report
open Printf


class report =
object(self)

  method private _dbus img title txt =
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


  method private _new_notification file filestate =
    let filename_escaped = Common.escape_for_notify file.f_name in
    let conf = (Config.cfg)#get in

    if conf.c_notify.n_locally then begin
      let msg_state =
	Common.string_of_filestate filestate
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

      let r = Str.regexp "/" in
      let l_folders = Str.split r file.f_path in

      let dbus_notif =
	match conf.c_notify.n_parent_folders with
	| None ->
	  sprintf "<b>%s</b> %s\n%s" file.f_username msg_state filename_escaped

	| Some parent_folders ->
	  sprintf "<b>%s</b> %s\n%s%s"
	    file.f_username
	    msg_state
	    (n_last_elements l_folders parent_folders)
	    filename_escaped
      in
      try
	self#_dbus "nobody" "Repwatcher" dbus_notif
      with _ -> Log.log ("An error occured with dBus", Error)
    end ;

    if conf.c_notify.n_remotely then
      try
	let str_notif =
	  Marshal.to_string
	    (New_notif ({file with f_name = filename_escaped}, filestate) )
	    [Marshal.No_sharing]
	in
	(* Send in the pipe for the server to send to the clients *)
	ignore ( Unix.write (Pipe.father2child#get_towrite) str_notif 0 (String.length str_notif) );
      with _ ->
	Log.log ("An error occured trying to send in the pipe the notification", Error)


  method private _local_notification txt2benotified =
    let txt_escaped = Common.escape_for_notify txt2benotified in
    let conf = (Config.cfg)#get in

    if conf.c_notify.n_locally then
      try
	self#_dbus "nobody" "Repwatcher" txt_escaped
      with _ -> Log.log ("An error occured with dBus", Error)


  method private _send_old_notifications2ssl_process current_accesses_l =
    (* Common.escape_for_notify has already been done on f_name *)
    let str_current =
	Marshal.to_string current_accesses_l [Marshal.No_sharing]
    in
    (* send through the pipe to be processed by the server *)
    ignore (Unix.write
	      (Pipe.father2child#get_towrite)
	      str_current
	      0
	      (String.length str_current))


  method notify notification =
    match notification with
      | New_notif (file, filestate) ->
	self#_new_notification file filestate

      | Local_notif txt2benotified ->
	self#_local_notification txt2benotified

      | Old_notif _ ->
	self#_send_old_notifications2ssl_process notification


  method sql
    ?(sql_obj_opt : Sqldb.sqldb option = None)
    sql_report =

    match Config.cfg#is_sql_activated with
      | false -> None
      | true ->

	match sql_report.sr_type with
	  | SQL_File_Opened ->
	    let sql = new Sqldb.sqldb in
	    sql#file_opened
	      sql_report.sr_common.c_file
	      sql_report.sr_common.c_written
	      sql_report.sr_common.c_opening_date#get_str_locale
	      sql_report.sr_common.c_filesize;
	    Some sql

	  | SQL_File_Closed ->
	    begin match sql_obj_opt with
	      | None -> assert false
	      | Some sql ->
		let closing_date_str =
		  match sql_report.sr_common.c_closing_date with
		  | None -> assert false
		  | Some closing_date -> closing_date#get_str_locale
		in
		sql#file_closed
		  closing_date_str
		  sql_report.sr_common.c_filesize
		  sql_report.sr_common.c_last_known_offset
		  sql_report.sr_common.c_written
	    end;
	    None

	  | SQL_FK_Offset ->
	    begin match sql_obj_opt with
	      | None -> assert false
	      | Some sql ->
		sql#first_known_offset sql_report.sr_common.c_first_known_offset
	    end;
	    None

	  | SQL_LK_Offset ->
	    begin match sql_obj_opt with
	      | None -> assert false
	      | Some sql ->
		sql#last_known_offset sql_report.sr_common.c_last_known_offset
	    end;
	    None

	  | SQL_Switch_On_Created ->
	    begin match sql_obj_opt with
	      | None -> assert false
	      | Some sql ->
		sql#switch_on_created
	    end;
	    None


  method mail tobemailed =

    if Config.cfg#is_instant_email_activated then
      let email_conf = Config.cfg#get_email in
      let email = new InstantMail.email tobemailed in
      match tobemailed.m_filestate with
      | (File_Opened | File_Created) ->
	if email_conf.e_open then
	  email#send ()

      | File_Closed ->
	if email_conf.e_close then
	  email#send ()
end ;;

let report = new report;;
