type f_file = {
  f_name        : string ;
  f_path        : string ;
  f_unix_login  : string ;

  (* f_username is the same than f_unix_login
   * if the real name is not set in /etc/passwd *)
  f_username    : string ;
  f_program     : string ;
  f_program_pid : Fdinfo.pid;
  f_descriptor  : Fdinfo.fd ;
}

type file_state = File_Created | File_Opened | File_Closed

type utc = Utc of string

type old_notif_t = {
  on_file : f_file ;
  on_opening_date_utc : utc
}

type notification =
  | New_notif of f_file * file_state
  | Old_notif of old_notif_t list
  | Local_notif of string


type com_server2clients =
  | Notification of notification
  | RW_server_exited
  | RW_server_con_ok of int option (* int is the notify_parent_folders value from the config file *)
;;
