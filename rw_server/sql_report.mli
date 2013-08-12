open Types;;

type sql_report = {
  s_file : f_file ;
  s_state : sql_type ;
  s_size : int64 option ;
  s_date : string ;
  s_offset : int64 option ;
  s_sql_obj : Sqldb.sqldb option ;
  s_created : bool ;
}
