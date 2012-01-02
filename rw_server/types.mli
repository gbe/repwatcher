type f_file = {
  f_name        : string ;
  f_path        : string ;
  f_login       : string ;
  f_program     : string ;
  f_program_pid : int    ;
  f_descriptor  : Fdinfo.fd ;
}

type file_state = File_Created | File_Opened | File_Closed

type log_level = | Normal
		 | Normal_Extra
		 | Error

(* file to send to clients, this way we don't send useless infos *)
type file2clients = {
  f2_name        : string ;
  f2_path        : string ;
  f2_login       : string ;
  f2_program     : string ;
}

type notification =
  | New_notif of file2clients * file_state
  | Old_notif of (file2clients * string) list
  | Local_notif of string


type sql_type =
  | SQL_File_Opened 
  | SQL_File_Closed
  | SQL_FK_Offset (* First Known *)
  | SQL_LK_Offset (* Last Known *)

type sql_report = {
  s_file : f_file ;
  s_state : sql_type ;
  s_size : int64 ;
  s_date : string ;
  s_offset : int64 option ;
  s_pkey : int64 option ;
  s_created : bool ;
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


