open Mysql
open Types
open Types_conf

exception Sql_not_connected
exception Sql_no_last_result
exception No_primary_key

type mysql_result = ResultOK | ResultEmpty | ResultError of string option

class mysqldb =
object(self)

  inherit Abstract_sql.abstract_sql


  method private _to_strsql int64opt =
    match int64opt with
      | None -> "NULL"
      | Some var_int64 -> ml2str (Int64.to_string var_int64)


  (* nodb = true only when creating the db *)
  method private _query ?(log=true) ?(nodb=false) ?(get_prim_key=false) q =
  
    if log then
      Log.log (("Next SQL query to compute:\n"^q^"\n"), Normal_Extra) ;


    (* Lock added since the discovery of the race in postgresqldb.ml *)
    Mutex.lock self#get_lock;

    if self#is_connected = false then begin
      match nodb with
	| false -> self#_connect () 
	| true -> self#connect_without_db
    end;

    try
      let cid' = self#_get_cid in
      ignore (exec cid' q);

      let status' = status cid' in

      begin match status' with
	| StatusOK ->
	  if log then
	    Log.log ("Query successfully executed OK", Normal_Extra);
	  last_result <- Some ResultOK

	| StatusEmpty ->	  
	  if log then
	    Log.log ("Query successfully executed Empty", Normal_Extra);
	  last_result <- Some ResultEmpty

	| StatusError _ ->
	  let errmsg' = errmsg cid' in
	  last_result <- Some (ResultError errmsg')
      end;

      if get_prim_key then
	begin match status' with
	  | (StatusOK | StatusEmpty) ->
	    primary_key <- Some (insert_id cid')
	  | _ -> ()
	end;
      
      self#disconnect ();
      Mutex.unlock self#get_lock

    with Mysql.Error error -> Log.log (error, Error)


  method private _connect ?(log=true) ?(nodb=false) () =

    try 
      let sqlparam = (Config.cfg)#get_sql in

      let cid' =
	match nodb, sqlparam.sql_dbport with
	  | false, None ->
	    Mysql.quick_connect
	      ~host:sqlparam.sql_dbhost
	      ~database:sqlparam.sql_dbname
	      ~password:sqlparam.sql_dbpwd
	      ~user:sqlparam.sql_dbuser
	      ()
	  | false, Some port ->
	    Mysql.quick_connect
	      ~host:sqlparam.sql_dbhost
	      ~database:sqlparam.sql_dbname
	      ~port:port
	      ~password:sqlparam.sql_dbpwd
	      ~user:sqlparam.sql_dbuser
	      ()
	  | true, None ->
	    Mysql.quick_connect
	      ~host:sqlparam.sql_dbhost
	      ~password:sqlparam.sql_dbpwd
	      ~user:sqlparam.sql_dbuser
	      ()
	  | true, Some port ->
	    Mysql.quick_connect
	      ~host:sqlparam.sql_dbhost
	      ~port:port
	      ~password:sqlparam.sql_dbpwd
	      ~user:sqlparam.sql_dbuser
	      ()
      in

      if log then
	Log.log ("Connected to MySQL", Normal_Extra) ;
      
      cid <- Some cid'
    with
      | Mysql.Error err ->
	Log.log (err, Error)
      | Config.SQL_not_configured ->
	let err = "Cannot connect to MySQL since it is not configured" in
	Log.log (err, Error)


  method connect_without_db =
    self#_connect ~nodb:true ()


  method disconnect ?(log=true) () =
    try
      let cid' = self#_get_cid in
      Mysql.disconnect cid';
      cid <- None ;
      
      if log then
	Log.log ("Disconnected from MySQL", Normal_Extra)

    with
      | Sql_not_connected ->
	Log.log ("Programming error: SQL not connected", Error)
      | Mysql.Error error ->
	Log.log ("RW could not disconnect from Mysql: "^error, Error)


  method create_db dbname =

    let q = "CREATE DATABASE IF NOT EXISTS "^dbname in
    self#_query ~nodb:true q;

    try
      match self#_get_last_result with
	| ResultOK ->
	  Log.log (("Database "^dbname^" successfully created"), Normal_Extra)

	| ResultEmpty -> ()

	| (ResultError errmsg') ->
	  begin match errmsg' with
	    | None ->
	      Log.log ("Oops. Mysqldb.create_db had an error and the RDBMS \
			doesn't know why", Error)
	    | Some err ->
	      Log.log (err, Error)
	  end ;
	  exit 2
    with Sql_no_last_result ->
      Log.log ("Something went wrong when resulting the last query", Error);
      exit 2


  method create_table_accesses =

    let q =
      Printf.sprintf "CREATE TABLE IF NOT EXISTS `accesses` (\
  `ID` int(10) NOT NULL AUTO_INCREMENT,\
  `LOGIN` varchar(20) NOT NULL,\
  `USERNAME` varchar(256) DEFAULT NULL,\
  `PROGRAM` varchar(26) NOT NULL,\
  `PROGRAM_PID` int(8) NOT NULL,\
  `PATH` varchar(512) NOT NULL,\
  `FILENAME` varchar(256) NOT NULL,\
  `FILESIZE` bigint(20) unsigned DEFAULT NULL,\
  `FILEDESCRIPTOR` int(7) unsigned NOT NULL,\
  `FIRST_KNOWN_OFFSET` bigint(20) unsigned DEFAULT NULL,\
  `LAST_KNOWN_OFFSET` bigint(20) unsigned DEFAULT NULL,\
  `OPENING_DATE` datetime NOT NULL,\
  `CLOSING_DATE` datetime DEFAULT NULL,\
  `CREATED` tinyint(1) unsigned NOT NULL,\
  `IN_PROGRESS` tinyint(1) unsigned NOT NULL,\
  PRIMARY KEY (`ID`),\
  INDEX in_progress_idx (`IN_PROGRESS`)\
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;"
    in
    
    try
      self#_query q;

      match self#_get_last_result with
	| (ResultOK | ResultEmpty) ->
	  Log.log (("Table accesses successfully created"), Normal_Extra)	 

	| ResultError err ->
	  begin
	    match err with
	      | None ->
		Log.log ("Oops. Mysqldb.create_table_accesses had an error and the RDBMS \
			     doesn't know why", Error)
		  
	      | Some errmsg' ->
		Log.log (errmsg', Error)
	  end ;
	  exit 2
    with Sql_no_last_result ->
      Log.log ("Something went wrong when resulting the last query", Error);
      exit 2


  method reset_in_progress =
    self#_query reset_accesses_query



  method file_opened f s_created creation_date filesize offset =
    let username =
      match Txt_operations.name f.f_login with
	| None -> "NULL"
	| Some username -> (ml2str username)
    in

    (* ml2str adds quotes. ml2str "txt" -> "'txt'" *)
    let query =
      Printf.sprintf "INSERT INTO accesses \
         (login, username, program, program_pid, path, filename, filesize, \
         filedescriptor, first_known_offset, opening_date, created, in_progress) \
	  VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, '1')"
        (ml2str f.f_login)
	username
	(ml2str f.f_program)
	(ml2int (Fdinfo.int_of_pid f.f_program_pid))
	(ml2str f.f_path)
	(ml2str f.f_name)
	(self#_to_strsql filesize)
	(ml2int (Fdinfo.int_of_fd f.f_descriptor))
	(self#_to_strsql offset)
	(ml2str creation_date)
	(ml2str (
	  match s_created with
	    | true -> "1"
	    | false -> "0"
	 )
	)
    in

    self#_query ~get_prim_key:true query ;

    match self#_get_last_result with
      | (ResultOK | ResultEmpty) ->
	Log.log ("MySQL: File successfully opened", Normal_Extra);
      | ResultError err ->
	match err with
	  | None ->
	    Log.log ("Oops. Mysql had an error when doing file_opened \
                       and cannot tell which one", Error)
	  | Some errmsg' -> Log.log (errmsg', Error)


  method file_closed closing_date filesize offset =

    try
      let pkey = self#_get_primary_key in

      let update_query =
	Printf.sprintf "UPDATE accesses \
        	  SET CLOSING_DATE = %s, FILESIZE = %s, \
                  LAST_KNOWN_OFFSET = %s, IN_PROGRESS = '0' \
        	  WHERE ID = %s"
	  (ml2str closing_date)
	  (self#_to_strsql filesize)
	  (self#_to_strsql offset)
	  (ml2str (Int64.to_string pkey))
      in

      self#_query update_query ;

      match self#_get_last_result with
	| (ResultOK | ResultEmpty) ->
	  Log.log (("MySQL: File successfully closed"), Normal_Extra)	 

	| ResultError err ->
	  match err with
	    | None ->
	      Log.log ("Oops. Mysql had an error when doing file_closed
                        and cannot tell which one", Error)
	    | Some errmsg' -> Log.log (errmsg', Error)
    with
      | No_primary_key ->
	Log.log ("SQL: cannot close file: no primary key", Error)
      | Sql_no_last_result ->
	Log.log ("Something went wrong when resulting the last query", Error)


  method private _update_known_offset ?(first=false) ?(last=false) offset =
    try
      let pkey = self#_get_primary_key in

      let field =
	match first, last with
	  | false, true -> "LAST_KNOWN_OFFSET"
	  | true, false -> "FIRST_KNOWN_OFFSET"
	  | _ -> assert false
      in

      let update_offset_query =
	Printf.sprintf "UPDATE accesses \
        	  SET %s = %s \
	          WHERE ID = %s"
	  field
	  (self#_to_strsql offset)
	  (ml2str (Int64.to_string pkey))
      in

      self#_query ~log:false update_offset_query;
      match self#_get_last_result with
	| (ResultOK | ResultEmpty) ->
	  let msg = "MySQL: File offset successfully updated ("^field^")" in
	  Log.log (msg, Normal_Extra)	 

	| ResultError err ->
	  match err with
	    | None ->
	      Log.log ("Oops. Mysql had an error when doing update_known_offset \
			and cannot tell which one", Error)
	    | Some errmsg' -> Log.log (errmsg', Error)
    with
      | No_primary_key ->
	Log.log ("SQL: cannot update the offset: no primary key", Error)
      | Sql_no_last_result ->
	Log.log ("Something went wrong when resulting the last query", Error)


  method first_known_offset offset =
    self#_update_known_offset ~first:true offset

  method last_known_offset offset =
    self#_update_known_offset ~last:true offset
     
end;;
