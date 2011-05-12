type f_file = {
  f_name        : string ;
  f_path        : string ;
  f_login       : string ;
  f_filesize    : int64  ;
  f_prog_source : string ;
}

type file_state = File_Opened | File_Closed;;

(* New of file
 * Old of file * date) list
 * Info of message (such as Repwatcher is watching you)
*)

type notification =
  | New_notif  of f_file * file_state
  | Old_notif  of (f_file * string) list
  | Local_notif of string

type com_server2clients =
  | Notification of notification
  | RW_server_exited
  | RW_server_con_ok of int option (* int is the notify_parent_folders value from the config file *)
;;
