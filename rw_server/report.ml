open Types
open Types_conf
open Printf

let prepare_data file =
  {
    f2_name = file.f_name ;
    f2_path = file.f_path ;
    f2_username =
      (match Txt_operations.name (file.f_login) with
	| None -> file.f_login
	| Some username -> username);
    f2_program = file.f_program ;
  }
;;

let dbus img title txt =
  IFNDEF NO_DBUS THEN

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

    ELSE
    ()
    END
;;




    
    
let notify notification =  
  
  match notification with
  | New_notif (file, filestate) ->
      begin

	let filename_escaped = Txt_operations.escape_for_notify file.f2_name in
	let conf = Config.get () in  


	if conf.c_notify.n_locally then begin
	  let msg_state =
	    match filestate with
	      | File_Created -> "has created"
	      | File_Opened -> "has opened"
	      | File_Closed -> "closed"
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
	  let l_folders = Str.split r file.f2_path in

	    
	  let dbus_notif =
	    match conf.c_notify.n_parent_folders with
	      | None ->
		sprintf "<b>%s</b> %s\n%s" file.f2_username msg_state filename_escaped 

	      | Some parent_folders ->
		sprintf "<b>%s</b> %s\n%s%s"
		  file.f2_username
		  msg_state
		  (n_last_elements l_folders parent_folders)
		  filename_escaped
	  in
	  dbus "nobody" "Repwatcher" dbus_notif

	end ;


	if conf.c_notify.n_remotely then
	  try
            
	    let str_notif =
	      Marshal.to_string
		(New_notif ({file with f2_name = filename_escaped}, filestate) )
		[Marshal.No_sharing]
	    in
	    
	    (* Send in the pipe for the server to send to the clients *)
	    ignore ( Unix.write Pipe.tow str_notif 0 (String.length str_notif) )
	  with _ ->
	    Log.log ("An error occured trying to send in the pipe the notification", Error)
	      
      end

  | Local_notif info ->
    let info_escaped = Txt_operations.escape_for_notify info in
    let conf = Config.get () in

    if conf.c_notify.n_locally then
      dbus "nobody" "Repwatcher" info_escaped


  | Old_notif l_current ->
      (* Txt_operations.escape_for_notify has already been done on f_name *)
      let str_current =
	Marshal.to_string (Old_notif l_current) [Marshal.No_sharing]
      in

      (* send through the pipe to be processed by the server *)
      ignore (Unix.write Pipe.tow str_current 0 (String.length str_current))
;;




let sql sql_report =

  let f = sql_report.s_file in
  
  let offset =
    match sql_report.s_offset with
      | None -> "NULL"
      | Some offset -> Mysqldb.ml2str (Int64.to_string offset)
  in

  let filesize =
    match sql_report.s_size with
      | None -> "NULL"
      | Some filesize -> Mysqldb.ml2str (Int64.to_string filesize)
  in

  let username =
    match Txt_operations.name f.f_login with
      | None -> "NULL"
      | Some username -> (Mysqldb.ml2str username)
  in

  match sql_report.s_state with
  | SQL_File_Opened  ->

    let created =
      match sql_report.s_created with
	| true -> "1"
	| false -> "0"
    in

    (* ml2str adds quotes. ml2str "txt" -> "'txt'" *)
    let query =
      Printf.sprintf "INSERT INTO accesses \
      (login, username, program, program_pid, path, filename, filesize, \
      filedescriptor, first_known_offset, opening_date, created, in_progress) \
	  VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, '1')"
        (Mysqldb.ml2str f.f_login)
	username
	(Mysqldb.ml2str f.f_program)
	(Mysqldb.ml2int (Fdinfo.int_of_pid f.f_program_pid))
	(Mysqldb.ml2str f.f_path)
	(Mysqldb.ml2str f.f_name)
	filesize
	(Mysqldb.ml2int (Fdinfo.int_of_fd f.f_descriptor))
	offset
	(Mysqldb.ml2str sql_report.s_date)
	(Mysqldb.ml2str created)
    in

    begin match Mysqldb.connect () with
      | None -> Nothing
      | Some cid ->
	ignore (Mysqldb.query cid query) ;
	let primary_key = Mysqldb.insert_id cid in
	Mysqldb.disconnect cid ;
	PrimaryKey primary_key
    end
	
	
  | SQL_File_Closed ->
    
    begin match sql_report.s_pkey with
      | None -> assert false
      | Some pkey ->
	
	let update_query =
	  Printf.sprintf "UPDATE accesses \
	  SET CLOSING_DATE = %s, FILESIZE = %s, \
          LAST_KNOWN_OFFSET = %s, IN_PROGRESS = '0' \
	  WHERE ID = %s"
	    (Mysqldb.ml2str sql_report.s_date)
	    filesize
	    offset
	    (Mysqldb.ml2str (Int64.to_string pkey))
	in
	
	match Mysqldb.connect () with
	  | None -> Nothing
	  | Some cid ->
	    ignore (Mysqldb.query cid update_query) ;
	    Mysqldb.disconnect cid ;
	    Nothing
    end


  | SQL_FK_Offset ->
    begin match sql_report.s_pkey with
      | None -> assert false
      | Some pkey ->
	
	let update_offset_query =
	  Printf.sprintf "UPDATE accesses \
	  SET FIRST_KNOWN_OFFSET = %s \
	  WHERE ID = %s"
	    offset
	    (Mysqldb.ml2str (Int64.to_string pkey))
	in
	
	match Mysqldb.connect () with
	  | None -> Nothing
	  | Some cid ->
	    ignore (Mysqldb.query cid update_offset_query) ;
	    Mysqldb.disconnect cid ;
	    Nothing
    end

  | SQL_LK_Offset ->
    match sql_report.s_pkey with
      | None -> assert false
      | Some pkey ->
	
	let update_offset_query =
	  Printf.sprintf "UPDATE accesses \
	  SET LAST_KNOWN_OFFSET = %s \
	  WHERE ID = %s"
	    offset
	    (Mysqldb.ml2str (Int64.to_string pkey))
	in
	
	match Mysqldb.connect () with
	  | None -> Nothing
	  | Some cid ->
	    ignore (Mysqldb.query cid update_offset_query) ;
	    Mysqldb.disconnect cid ;
	    Nothing
;;




module Report =
struct
  
  let prepare_data file = prepare_data file ;;

  let report = function
    | Sql sql_report ->
      sql sql_report

    | Notify notification ->
      notify notification ;
      Nothing
      
;;
end;;
