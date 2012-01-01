type file2clients = {
  f2_name        : string ;
  f2_path        : string ;
  f2_login       : string ;
  f2_program     : string ;
}

type file_state = File_Created | File_Opened | File_Closed

(* New of file
 * Old of file * date) list
 * Info of message (such as Repwatcher is watching you)
*)

type notification =
  | New_notif  of file2clients * file_state
  | Old_notif  of (file2clients * string) list
  | Local_notif of string

type com_server2clients =
  | Notification of notification
  | RW_server_exited
  | RW_server_con_ok of int option (* int is the notify_parent_folders value from the config file *)
;;
