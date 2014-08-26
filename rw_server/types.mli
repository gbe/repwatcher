(* The f_file type must be static as it is
 * used as a hashtbl key *)
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

type common_t = {
  c_file : f_file;
  c_filesize : int64 option;
  c_first_known_offset : int64 option;
  c_last_known_offset : int64 option;
  c_opening_date : Date.date;
  c_closing_date : Date.date option;
  c_written : bool;
}

type file_state = | File_Created
		  | File_Opened
		  | File_Closed


type log_level = | Normal
		 | Normal_Extra
		 | Error


type old_notif_t = {
  on_file : f_file ;
  on_opening_date_utc : Types_date.utc
}

type notification =
  | New_notif of f_file * file_state
  | Old_notif of old_notif_t list
  | Local_notif of string


type sql_type =
  | SQL_File_Opened
  | SQL_File_Closed
  | SQL_FK_Offset (* First Known *)
  | SQL_LK_Offset (* Last Known *)
  | SQL_Switch_On_Created


type mail_t = {
  m_common : common_t;
  m_filestate: file_state;
}

type com_net2main =
  | Log of (string * log_level)
  | Ask_current_accesses
  | Exit_on_error of string

type com_server2clients =
  | Notification of notification
  | RW_server_exited
  | RW_server_con_ok of int option (* int is the notify_parent_folders value from the config file *)
