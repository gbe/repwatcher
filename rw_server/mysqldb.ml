open Mysql
open Types
open Types_conf
open Unix

exception Mysql_not_connected

class mysqldb =
object(self)

  val mutable cid = None
  val dbparam = (Config.cfg)#get_sql

  method private _get_cid =
    match cid with
      | None -> raise Mysql_not_connected
      | Some cid' -> cid'

  method private _to_strsql int64opt =
    match int64opt with
      | None -> "NULL"
      | Some var_int64 -> ml2str (Int64.to_string var_int64)


(*  method private _map res =
    try
      (* row = string option array, the option is if a field is NULL *)
      let rows = Mysql.map res (fun row -> row) in
      Log.log ("Query successfully mapped", Normal_Extra);

      rows

    with Mysql.Error error ->
      Log.log (error, Error);
      []
*)

  method private _query ?(log=true) q =
  
    if log then
      Log.log (("Next SQL query to compute:\n"^q^"\n"), Normal_Extra) ;

    if self#is_connected = false then
      self#_connect ();

    try
      let cid = self#_get_cid in
      ignore (exec cid q);
    
      match status cid with
	| (StatusOK | StatusEmpty) ->
	  if log then
	    Log.log ("Query successfully executed", Normal_Extra)

	| StatusError _ ->
	  begin match errmsg cid with
	    | None ->
	      Log.log ("Oops. Mysqldb.query had an error and the RDBMS \
		       cannot tell which one", Error)
	    | Some errmsg' -> Log.log (errmsg', Error)
	  end

    with
      | Mysql_not_connected ->
	Log.log ("Programming error: SQL not connected", Error)
      | Mysql.Error error ->
	Log.log (error, Error)


  method private _connect ?(log=true) ?(nodb=false) () =

    let dbparam' = match nodb with
      | false -> dbparam
      | true -> { dbparam with dbname = None }
    in

    try
      let cid' = Mysql.connect dbparam' in

      if log then
	Log.log ("Connected to MySQL", Normal_Extra) ;
      
      cid <- Some cid'
    with
      | Mysql.Error error ->
	Log.log (error, Error)
      | Config.Config_error -> ()


  method connect_without_db =
    self#_connect ~nodb:true ()

  method is_connected =
    match cid with
      | None -> false
      | Some _ -> true

  method disconnect ?(log=true) () =
    try
      let cid' = self#_get_cid in
      Mysql.disconnect cid';
      cid <- None ;
      
      if log then
	Log.log ("Disconnected from MySQL", Normal_Extra)

    with
      | Mysql_not_connected ->
	Log.log ("Programming error: SQL not connected", Error)
      | Mysql.Error error ->
	Log.log ("RW could not disconnect from Mysql: "^error, Error)


  method create_db dbname =

    if self#is_connected = false then
      self#connect_without_db;

    let q = "CREATE DATABASE IF NOT EXISTS "^dbname in

    try

      let cid = self#_get_cid in
      ignore (exec cid q);

      begin
	match status cid with
	  | StatusOK ->
	    Log.log (("Database "^dbname^" successfully created"), Normal_Extra)

	  | StatusEmpty -> ()

	  | StatusError _ ->
	    begin
	      match errmsg cid with
		| None ->
		  Log.log ("Oops. Mysqldb.create_db had an error and the RDBMS \
			     doesn't know why", Error)
		    
		| Some errmsg' ->
		  Log.log (errmsg', Error)
	    end ;
	    exit 2
      end ;

      self#disconnect ()

    with
      | Mysql_not_connected ->
	Log.log ("Programming error: SQL not connected", Error);
	exit 2
      | Mysql.Error error ->
	Log.log (error, Error) ;
	exit 2

  method create_table_accesses =
    if self#is_connected = false then
      self#_connect ();

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
      let cid = self#_get_cid in
      ignore (exec cid q);

      begin
	match status cid with
	  | (StatusOK | StatusEmpty) ->
	    Log.log (("Table accesses successfully created"), Normal_Extra)	 

	  | StatusError _ ->
	    begin
	      match errmsg cid with
		| None ->
		  Log.log ("Oops. Mysqldb.create_table_accesses had an error and the RDBMS \
			     doesn't know why", Error)

		| Some errmsg' ->
		  Log.log (errmsg', Error)
	    end ;
	    exit 2
      end ;

      self#disconnect ()

    with
      | Mysql_not_connected ->
	Log.log ("Programming error: SQL not connected", Error);
	exit 2
      | Mysql.Error error ->
	Log.log (error, Error) ;
	exit 2


  method reset_in_progress =
    let reset_accesses =
      "UPDATE accesses SET IN_PROGRESS = '0' WHERE IN_PROGRESS = '1'"
    in

    if self#is_connected = false then
      self#_connect ();
  
    self#_query reset_accesses;
    self#disconnect ()



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

    if self#is_connected = false then
      self#_connect ();

    self#_query query ;

    try
      let cid' = self#_get_cid in
      let primary_key = insert_id cid' in
      self#disconnect () ;
      PrimaryKey primary_key
    with Mysql_not_connected ->
      Log.log ("Mysql file opened could not be executed - Not connected", Error);
      Nothing


  method file_closed pkey closing_date filesize offset = 
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

    if self#is_connected = false then
      self#_connect ();

    self#_query update_query ;	
    self#disconnect () ;
    Nothing

  method private _update_known_offset ?(first=false) ?(last=false) pkey offset =
    let field =
      match first, last with
	| false, false -> assert false
	| false, true -> "LAST_KNOWN_OFFSET"
	| true, false -> "FIRST_KNOWN_OFFSET"
	| true, true -> assert false
    in

    let update_offset_query =
      Printf.sprintf "UPDATE accesses \
        	  SET %s = %s \
	          WHERE ID = %s"
	field
	(self#_to_strsql offset)
	(ml2str (Int64.to_string pkey))
    in

    if self#is_connected = false then
      self#_connect ~log:false ();

    self#_query ~log:false update_offset_query ;
    self#disconnect ~log:false () ;
    Nothing


  method first_known_offset pkey offset =
    self#_update_known_offset ~first:true pkey offset

  method last_known_offset pkey offset =
    self#_update_known_offset ~last:true pkey offset

end;;
