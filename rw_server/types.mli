type f_file = {
  f_name        : string ;
  f_path        : string ;
  f_login       : string ;
  f_filesize    : int64  ;
  f_program     : string ;
  f_program_pid : int    ;
}

type file_state = File_Opened | File_Closed

type log_level = | Normal
		 | Normal_Extra
		 | Error


type notification =
  | New_notif  of f_file * file_state
  | Old_notif  of (f_file * string) list
  | Local_notif of string


type sql_type =
  | SQL_File_Opened 
  | SQL_File_Closed
  | SQL_File_Offset_Updated

type sql_report = {
  s_file : f_file ;
  s_state : sql_type ;
  s_date : string ;
  s_offset : int64 option ;
  s_pkey : int64 option ;
}

type report = | Notify of notification
	      | Sql    of sql_report

type report_ret = Nothing | PrimaryKey of int64

type com_net2main =
  | Log of (string * log_level)
  | Ask_current_accesses
  | Exit_on_error of string

type com_server2clients =
  | Notification of notification
  | RW_server_exited
  | RW_server_con_ok of int option (* int is the notify_parent_folders value from the config file *)


