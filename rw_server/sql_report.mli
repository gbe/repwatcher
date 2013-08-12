open Types;;

type dbkind = Mysql of Mysqldb.mysqldb | Postgresql of Mysqldb.mysqldb

type sql_report = {
  s_file : f_file ;
  s_state : sql_type ;
  s_size : int64 option ;
  s_date : string ;
  s_offset : int64 option ;
  s_sql_obj : dbkind option ;
  s_created : bool ;
}
