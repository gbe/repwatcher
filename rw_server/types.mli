type f_file = {
  f_name            : string ;
  f_path            : string ;
  f_unix_login : string ;

  (* f_username could be the same than f_unix_login
   * if not set in /etc/passwd *)
  f_username        : string ; 
  f_program         : string ;
  f_program_pid     : Fdinfo.pid;
  f_descriptor      : Fdinfo.fd ;
}


type file_state = | File_Created
		  | File_Opened
		  | File_Closed


type log_level = | Normal
		 | Normal_Extra
		 | Error


type notification =
  | New_notif of f_file * file_state
  | Old_notif of (f_file * Date.date) list
  | Local_notif of string


type sql_type =
  | SQL_File_Opened 
  | SQL_File_Closed
  | SQL_FK_Offset (* First Known *)
  | SQL_LK_Offset (* Last Known *)
  | SQL_Switch_On_Created


type mail_t = {
  m_filestate: file_state;
  m_file: f_file;
  m_first_offset: int64 option;
  m_last_offset: int64 option;
  m_filesize: int64 option;
  m_opening_date: Date.date option;
  m_closing_date: Date.date option;
}

type com_net2main =
  | Log of (string * log_level)
  | Ask_current_accesses
  | Exit_on_error of string

type com_server2clients =
  | Notification of notification
  | RW_server_exited
  | RW_server_con_ok of int option (* int is the notify_parent_folders value from the config file *)
