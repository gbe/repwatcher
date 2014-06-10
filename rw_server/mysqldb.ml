open Mysql
open Types
open Types_conf
open Abstract_sql

type mysql_result = ResultOK | ResultEmpty | ResultError of string option

class mysqldb =
object(self)

  inherit Abstract_sql.abstract_sql

  method private _query 
    ?(log=true) 
    ?(nodb=false)  (* nodb = true only when creating the db *)
    ?(save_prim_key=false) 
    ?(disconnect=false) 
    ?(args=[||]) 
    (tquery, q) =

    if log then
      begin
	let txt =
	  self#_args_list_to_string
	    ("Next SQL query to compute: "^q^" --- Args: ")
	    (Array.to_list args)
	in
	Log.log (txt, Normal_Extra);
      end;

    (* Lock added since the discovery of the race in postgresqldb.ml *)
    Mutex.lock self#_get_lock;

    if self#is_connected = false then begin
      match nodb with
	| false -> self#_connect () 
	| true -> self#connect_without_db
    end;

    try
      let cid' = self#_get_cid in

      let create_statement statement_var =
	match !statement_var with
	  | None ->
	    let st = Prepared.create cid' q in
	    Log.log ("Prepared statement computed", Normal_Extra);
	    statement_var := Some (Mysqlst st);
	    st
	  | Some Mysqlst st -> st
	  | Some Postgresqlst -> assert false
      in

      let statemt =
	match tquery with
	  | CreateDb -> create_statement stmt_create_db
	  | CreateTable -> create_statement stmt_create_table
	  | UpdateResetProgress -> create_statement stmt_update_reset_progress
	  | InsertOpen -> create_statement stmt_insert_open
	  | UpdateFirstOffset -> create_statement stmt_update_first_offset
	  | UpdateFirstOffset_Null -> create_statement stmt_update_first_offset_null
	  | UpdateLastOffset -> create_statement stmt_update_last_offset
	  | UpdateLastOffset_Null -> create_statement stmt_update_last_offset_null
	  | UpdateCreated -> create_statement stmt_update_created
	  | UpdateClose -> create_statement stmt_update_close
	  | (SelectIndexExists|SelectDbExists|CreateIndex) ->
	    assert false (* postgresql's cases *)
      in

      ignore (Prepared.execute statemt args);

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

      if save_prim_key then
	begin match status' with
	  | (StatusOK | StatusEmpty) ->
	    primary_key <- Some (Prepared.insert_id statemt)
	  | _ -> ()
	end;

      (* The disconnection is performed on a by-object basis and
       * when occurs the events :
       * - create database
       * - reset_progress
       * - the file_close *)
      if disconnect then
	self#disconnect ();

      Mutex.unlock self#_get_lock

    with
      | Mysql.Error error -> Log.log (error, Error)
      | Sql_not_connected -> Log.log ("MySQL error: object not connected, cannot query", Error)


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
	let err = "MySQL error: cannot connect since it is not configured" in
	Log.log (err, Error)


  method connect_without_db =
    self#_connect ~nodb:true ()


  method disconnect ?(log=true) () =
    self#_cleanup_prepare_stmts;

    try
      let cid' = self#_get_cid in
      Mysql.disconnect cid';
      cid <- None ;
      
      if log then
	Log.log ("Disconnected from MySQL", Normal_Extra)

    with
      | Sql_not_connected ->
	Log.log ("MySQL error: object not connected, cannot disconnect", Error)
      | Mysql.Error error ->
	Log.log ("MySQL error: RW could not disconnect: "^error, Error)


  method create_db dbname =

    let q = "CREATE DATABASE IF NOT EXISTS "^dbname in
    self#_query 
      ~nodb:true 
      ~disconnect:true 
      (CreateDb, q);

    try
      match self#_get_last_result with
	| ResultOK ->
	  Log.log (("Database "^dbname^" successfully created or already existing"), Normal_Extra)

	| ResultEmpty -> ()

	| (ResultError errmsg') ->
	  begin match errmsg' with
	    | None ->
	      Log.log ("MySQL error: create_db had an error and \
			the RDBMS does not know why", Error)
	    | Some err ->
	      Log.log (err, Error)
	  end ;
	  exit 2
    with Sql_no_last_result ->
      Log.log ("MySQL error: something went wrong when resulting the last query", Error);
      exit 2


  method create_table_accesses =

    let q =
  "CREATE TABLE IF NOT EXISTS `accesses` (\
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
      (* No need to disconnect after this query as
       * the same object is used to reset_in_progress.
       * The disconnection is performed after the reset_in_progress *)
      self#_query (CreateTable, q);

      match self#_get_last_result with
	| (ResultOK | ResultEmpty) ->
	  Log.log (("Table accesses successfully created or already existing"), Normal_Extra)

	| ResultError err ->
	  begin
	    match err with
	      | None ->
	      Log.log ("MySQL error: create_table_accesses had an error and \
			the RDBMS does not know why", Error)
		  
	      | Some errmsg' ->
		Log.log (errmsg', Error)
	  end ;
	  exit 2
    with Sql_no_last_result ->
      Log.log ("MySQL error: something went wrong when resulting the last query", Error);
      exit 2


  method reset_in_progress =
    self#_query 
      ~disconnect:true
      (UpdateResetProgress, reset_accesses_query)


  method file_opened f s_created creation_date filesize =
    let query = ref
      "INSERT INTO accesses \
      (login, username, program, program_pid, path, filename, "
    in
    let args = 
      ref [
        f.f_unix_login;
	(match Txt_operations.name f.f_unix_login with
	  | None -> "NULL"
	  | Some username -> username);
	f.f_program;
	string_of_int (Fdinfo.int_of_pid f.f_program_pid);
	f.f_path;
	f.f_name]
    in

    if not (filesize = None) then begin
      query := !query^"filesize, ";
      args := (!args) @ [self#_to_strsql filesize];
    end;


    query := !query^"filedescriptor, opening_date, created, in_progress) \
       VALUES (?, ?, ?, ?, ?, ?,";

    if not (filesize = None) then
      query := !query^"?, ";

    query := !query^"?, ?, ?, '1')";

    args := 
	!args @ [string_of_int (Fdinfo.int_of_fd f.f_descriptor);
		 creation_date;
		 (match s_created with
		   | true -> "1"
		   | false -> "0"
		 )
		];

    self#_query
      ~save_prim_key:true
      ~args:(Array.of_list !args)
      (InsertOpen, !query) ;

    try
      match self#_get_last_result with
	| (ResultOK | ResultEmpty) ->
	  Log.log ("MySQL: File successfully opened", Normal_Extra);
	| ResultError err ->
	  match err with
	    | None ->
	      Log.log ("MySQL error when doing file_opened \
                       and the RDBMS cannot tell which one", Error)
	    | Some errmsg' -> Log.log (errmsg', Error)
    with Sql_no_last_result ->
      let err = "RW could not get any result from MySQL \
                 after the event file_opened on file "^f.f_name 
      in
      Log.log (err, Error)


  method file_closed closing_date filesize offset created =

    try
      let pkey = self#_get_primary_key in

      (* Workaround the bug about NULL parameter in MySQL prepared statements *)
      let update_query = ref
	"UPDATE accesses \
         SET CLOSING_DATE = ?, "
      in
      let args = ref [closing_date] in

      if not (filesize = None) then begin
	update_query := !update_query^"FILESIZE = ?, ";
	args := (!args) @ [self#_to_strsql filesize];
      end;
	
      if not (offset = None) then begin
        update_query := !update_query^"LAST_KNOWN_OFFSET = ?, ";
	  args := (!args) @ [self#_to_strsql offset]
      end;

      update_query := !update_query^"CREATED = ?, IN_PROGRESS = '0' WHERE ID = ?"; 

      args :=
	(!args) @
	[(match created with
	  | true -> "1"
	  | false -> "0"
	 );
	 Int64.to_string pkey] ;      

      self#_query
	~disconnect:true
	~args:(Array.of_list !args)
	(UpdateClose, !update_query) ;

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
	Log.log ("MySQL error: cannot close file, no primary key", Error)
      | Sql_no_last_result ->
	Log.log ("MySQL error: something went wrong when resulting the last query", Error)


  method private _update_known_offset ?(first=false) ?(last=false) offset =
    try
      let pkey = self#_get_primary_key in

      let (tquery, field) =
	match first, last, offset with
	  | false, true, None -> (UpdateLastOffset_Null, "LAST_KNOWN_OFFSET")
	  | false, true, Some _ -> (UpdateLastOffset, "LAST_KNOWN_OFFSET")
	  | true, false, None -> (UpdateFirstOffset_Null, "FIRST_KNOWN_OFFSET")
	  | true, false, Some _ -> (UpdateFirstOffset, "FIRST_KNOWN_OFFSET")
	  | _ -> assert false
      in

      let update_offset_query =
	"UPDATE accesses \
         SET "^field^" = "^
	  (match offset with
	    | None -> "NULL"
	    | Some _ -> "?"
	  )^" WHERE ID = ?"
      in


      let args =
	if offset = None then
	  [| Int64.to_string pkey |]
	else
	  [|
	    self#_to_strsql offset;
	    Int64.to_string pkey
	  |]
      in

      self#_query
	~log:false
	~args:args
	(tquery, update_offset_query);

      match self#_get_last_result with
	| (ResultOK | ResultEmpty) ->
	  let msg = "MySQL: File offset successfully updated ("^field^")" in
	  Log.log (msg, Normal_Extra)	 

	| ResultError err ->
	  match err with
	    | None ->
	      Log.log ("MySQL error when doing update_known_offset \
			and the RDBMS cannot tell which one", Error)
	    | Some errmsg' -> Log.log (errmsg', Error)
    with
      | No_primary_key ->
	Log.log ("MySQL error: cannot update the offset: no primary key", Error)
      | Sql_no_last_result ->
	Log.log ("MySQL error: something went wrong when resulting the last query", Error)


  method first_known_offset offset =
    self#_update_known_offset ~first:true offset


  method last_known_offset offset =
    self#_update_known_offset ~last:true offset


  method switch_on_created =
    try
      let pkey = self#_get_primary_key in

      let switch_on_created_query =
	"UPDATE accesses \
         SET CREATED = '1', FILESIZE = NULL \
	 WHERE ID = ?"
      in
      let switch_on_created_query_arg =
	[| Int64.to_string pkey |]
      in

      self#_query
	~log:true
	~args:switch_on_created_query_arg
	(UpdateCreated, switch_on_created_query);

      match self#_get_last_result with
	| (ResultOK | ResultEmpty) ->
	  let msg =
	    "MySQL: Field Created successfully switched on \
             for primary key: "^(Int64.to_string pkey)
	  in
	  Log.log (msg, Normal_Extra)	 

	| ResultError err ->
	  match err with
	    | None ->
	      Log.log ("MySQL error when switching 'on' the field Created \
			and the RDBMS cannot tell which one", Error)
	    | Some errmsg' -> Log.log (errmsg', Error)
    with No_primary_key ->
      Log.log ("MySQL error: cannot switch 'on' the field Created, no primary key", Error)

end;;
