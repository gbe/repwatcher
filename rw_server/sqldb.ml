type dbkind = Mysql of Mysqldb.mysqldb | Postgresql of Postgresqldb.pgsql

class sqldb =
object(self)
  inherit Abstract_sql.abstract_sql

  val connector =
    match Config.cfg#get_sql_rdbms with
      | "postgresql" -> Postgresql (new Postgresqldb.pgsql)
      | "mysql" -> Mysql (new Mysqldb.mysqldb)
      | _ -> assert false

  method is_connected =
    match connector with
      | Mysql con ->
	con#is_connected
      | Postgresql con ->
	con#is_connected


  method connect_without_db =
    match connector with
      | Mysql con ->
	con#connect_without_db
      | Postgresql con ->
	con#connect_without_db


  method disconnect ?(log=true) () =
    match connector with
      | Mysql con ->
	con#disconnect ~log:log ()
      | Postgresql con ->
	con#disconnect ~log:log ()

  method file_opened f s_created creation_date filesize offset =
    match connector with
      | Mysql con ->
	con#file_opened f s_created creation_date filesize offset
      | Postgresql con ->
	con#file_opened f s_created creation_date filesize offset

  method file_closed closing_date filesize offset =
    match connector with
      | Mysql con ->
	con#file_closed closing_date filesize offset
      | Postgresql con ->
	con#file_closed closing_date filesize offset

  method first_known_offset offset =
    match connector with
      | Mysql con ->
	con#first_known_offset offset
      | Postgresql con ->
	con#first_known_offset offset

  method last_known_offset offset =
    match connector with
      | Mysql con ->
	con#last_known_offset offset
      | Postgresql con ->
	con#last_known_offset offset

  method reset_in_progress =
    match connector with
      | Mysql con ->
	con#reset_in_progress
      | Postgresql con ->
	con#reset_in_progress

  method create_db dbname =
    match connector with
      | Mysql con ->
	con#create_db dbname
      | Postgresql con ->
	con#create_db dbname

  method create_table_accesses =
    match connector with
      | Mysql con ->
	con#create_table_accesses
      | Postgresql con ->
	con#create_table_accesses

end;;
