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
	  DBus.String title; (* Title of the notification window *)
	  DBus.String txt; (* The content *)
	  DBus.Array (DBus.Strings []);
	  DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigVariant), []));
	  DBus.Int32 15000l; (* milliseconds the notification must be seen *)
	] in	
	  ignore (send_notif_msg ~bus ~serv:"Notify" ~params)
;;

