let print_dbus_ty_list l =
	List.iter (fun o -> Printf.printf "%s\n" (DBus.string_of_ty o)) l
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
	  DBus.String "n";
	  DBus.UInt32 1l;
	  DBus.String img;
	  DBus.String title;
	  DBus.String txt;
	  DBus.Array (DBus.Strings []);
	  DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigVariant), []));
	  DBus.Int32 4000l;
	] in	
	  let r = send_notif_msg ~bus ~serv:"Notify" ~params in
	  print_dbus_ty_list r;
	  ()
;;

