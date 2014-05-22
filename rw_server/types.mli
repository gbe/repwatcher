type f_file = {
  f_name        : string ;
  f_path        : string ;
  f_login       : string ;
  f_program     : string ;
  f_program_pid : Fdinfo.pid;
  f_descriptor  : Fdinfo.fd ;
}

type file_state = | File_Created
		  | File_Opened
		  | File_Closed

type log_level = | Normal
		 | Normal_Extra
		 | Error

(* file to send to clients, this way we don't send useless infos *)
type file2clients = {
  f2_name        : string ;
  f2_path        : string ;
  f2_username    : string ;
  f2_program     : string ;
}

type notification =
  | New_notif of file2clients * file_state
  | Old_notif of (file2clients * CalendarLib.Calendar.t) list
  | Local_notif of string


type sql_type =
  | SQL_File_Opened 
  | SQL_File_Closed
  | SQL_FK_Offset (* First Known *)
  | SQL_LK_Offset (* Last Known *)
  | SQL_Switch_On_Created



type mail_t = {
  m_filestate: file_state;
  m_file: file2clients;
  m_offset: int64 option;
  m_filesize: int64 option;
  m_opening_date: string option;
  m_closing_date: string option;
}

type com_net2main =
  | Log of (string * log_level)
  | Ask_current_accesses
  | Exit_on_error of string

type com_server2clients =
  | Notification of notification
  | RW_server_exited
  | RW_server_con_ok of int option (* int is the notify_parent_folders value from the config file *)


