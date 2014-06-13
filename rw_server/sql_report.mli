open Types;;

type sql_report_t = {
  s_file : f_file ;
  s_state : sql_type ;
  s_filesize : int64 option ;
  s_date : string ;
  s_first_offset : int64 option ;
  s_last_offset : int64 option ;
  s_written : bool ;
}
