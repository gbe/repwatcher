open Postgresql
open Types
open Types_conf
open Abstract_sql

class pgsql =
object(self)

  inherit Abstract_sql.abstract_sql

  val mutable host = None
  val mutable port = None
  val mutable dbname = None
  val mutable user = None
  val mutable pwd = None
  val mutable requiressl = None


  initializer
  let sqlparam = (Config.cfg)#get_sql in

  host <- Some sqlparam.sql_dbhost ;
  dbname <- Some sqlparam.sql_dbname ;
  user <- Some sqlparam.sql_dbuser ;
  pwd <- Some sqlparam.sql_dbpwd ;
  port <-
    (match sqlparam.sql_dbport with
      | None -> None
      | Some p -> Some (string_of_int p))


  method private _get_dbname =
    match dbname with
      | None -> assert false
      | Some dbname' -> dbname'


  method private _get_connected_dbname =
    (* No need to test if it's connected as the exception 
     * will take care of the "not connected" case anyway *)
    try
      (self#_get_cid)#db
    with 
      | Sql_not_connected ->
	Log.log ("PgSQL error: object not connected, cannot get database name", Error);
	""
      | Postgresql.Error e ->
	Log.log (string_of_error e, Error);
	""
      | e ->
	Log.log (Printexc.to_string e, Error);
	""


  method private _connect ?(log=true) ?(nodb=false) () =

    (* code copy/pasted from the lib postgresql-ocaml *)
    let conninfo =
      let b = Buffer.create 512 in
      let field name = function
	| None -> ()
	| Some x ->
          Printf.bprintf b "%s='" name;
          for i = 0 to String.length x - 1 do
            if x.[i]='\''
            then Buffer.add_string b "\\'"
            else Buffer.add_char b x.[i]
          done;
          Buffer.add_string b "' "
      in
      field "host" host;
      field "hostaddr" None;
      field "port" port;
      field "dbname" (match nodb with false -> dbname | true -> Some "template1");
      field "user" user;
      field "password" pwd;
      field "options" None;
      field "tty" None;
      field "requiressl" requiressl;
      Buffer.contents b
    in
    (* end of copy/paste *)

    try
      let c =
	new connection ~conninfo:conninfo ()
      in
      if log then
	Log.log ("Connected to PostgreSQL", Normal_Extra) ;
      cid <- Some c
    with
      | Postgresql.Error e -> Log.log (string_of_error e, Error)
      | e -> Log.log (Printexc.to_string e, Error)


  method connect_without_db =
    self#_connect ~nodb:true ()


  (* The disconnection is performed on a by-object basis and
   * when occurs the events :
   * - create database
   * - reset_progress
   * - the file_close
   *)
  method disconnect ?(log=true) () =
    self#_cleanup_prepare_stmts;

    try
      (self#_get_cid)#finish ;
      cid <- None;

      if log then
	Log.log ("Disconnected from PostgreSQL", Normal_Extra);

    with
      | Sql_not_connected ->
	Log.log ("PgSQL error: object not connected, cannot disconnect", Error)
      | Postgresql.Error e -> Log.log ("PgSQL error: "^(string_of_error e), Error)
      | e -> Log.log (Printexc.to_string e, Error)



  method private _query
    ~expect
    ?(log=true)
    ?(nodb=false)
    ?(disconnect=false)
    ?(args=[||])
    (tquery, q) =
    
    if log then
      begin
	let txt =
	  self#_args_list_to_string
	    ("PgSQL: next SQL query to compute: "^q^" --- Args: ")
	    (Array.to_list args)
	in
	Log.log (txt, Normal_Extra);
      end;

    (* Lock proven to be useful because of the offset_thread using the very same object
     * There was a concurrency between the 2 threads.
     * One disconnected from postgres while the other thread thought to be still connected
     * while executing the query.
     * It resulted in the error: Failure("Postgresql.check_null: connection already finished")
     *)
    Mutex.lock self#_get_lock;

    if self#is_connected = false then begin
      match nodb with
	| false -> self#_connect ~log:log ()
	| true -> self#connect_without_db
    end;
	  
    try

      let cid' = self#_get_cid in

      let create_statement stmt_var stmt_str =
	match !stmt_var with
	  | None ->
	    let res = cid'#prepare stmt_str q in
	    last_result <- Some res;
	    begin match res#status with
	      | Command_ok ->
		Log.log ("Prepared statement '"^stmt_str^"' computed", Normal_Extra);
		stmt_var := Some Postgresqlst;
		(true, stmt_str);
	      | _ ->
		Log.log (("PgSQL error: "^res#error), Error);
		(false, stmt_str);
	    end
	  | Some Postgresqlst -> (true, stmt_str)
	  | Some Mysqlst _ -> assert false
      in

      let (statemt_computed, statemt_str) =
	match tquery with
	  | SelectDbExists -> create_statement stmt_db_exists "stmt_select_db_exists"
	  | CreateDb -> create_statement stmt_create_db "stmt_create_db"
	  | CreateTable -> create_statement stmt_create_table "stmt_create_table"
	  | SelectIndexExists -> create_statement stmt_idx_exists "stmt_select_index_exists"
	  | CreateIndex -> create_statement stmt_create_idx "stmt_create_index"
	  | InsertOpen -> create_statement stmt_insert_open "stmt_insert_open"
	  | UpdateFirstOffset -> create_statement
	    stmt_update_first_offset
	    "stmt_update_first_offset"
	  | UpdateLastOffset -> create_statement
	    stmt_update_last_offset
	    "stmt_update_last_offset"
	  | UpdateCreated -> create_statement stmt_update_created "stmt_update_created"
	  | UpdateClose -> create_statement stmt_update_close "stmt_update_close"
	  | UpdateResetProgress -> create_statement
	    stmt_update_reset_progress
	    "stmt_update_reset_progress"
	  | (UpdateFirstOffset_Null | UpdateLastOffset_Null) -> assert false (* MySQL workarounds *)
      in

      begin match statemt_computed with
	| false -> (* Statement not correctly computed *)
	  Log.log ("PgSQL error: statement not executed since the preparation failed", Error)
	| true ->
	  last_result <- Some (cid'#exec_prepared ~expect:[expect] ~params:args statemt_str);
      end;

      if disconnect then
	self#disconnect ~log:log ();

      Mutex.unlock self#_get_lock
    with 
      | Sql_not_connected ->
	Log.log ("PgSQL error: object not connected, cannot query", Error)
      | Postgresql.Error e -> Log.log (string_of_error e, Error)
      | e -> Log.log (Printexc.to_string e, Error)



  method file_opened f s_created creation_date filesize =
    let insert_query =
      "INSERT INTO accesses \
       (login, username, program, program_pid, path, filename, \
        filesize, filedescriptor, opening_date, created, in_progress) \
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, '1') \
       RETURNING ID"
    in
    let insert_query_args =
      [|
	f.f_unix_login;
	f.f_username;
	f.f_program;
	(string_of_int (Fdinfo.int_of_pid f.f_program_pid));
	f.f_path;
	f.f_name;
	(self#_to_strsql filesize);
	(string_of_int (Fdinfo.int_of_fd f.f_descriptor));
	creation_date;
	(match s_created with
	  | true -> "1"
	  | false -> "0"
	)
      |]
    in
    self#_query
      ~expect:Tuples_ok
      ~args:insert_query_args
      (InsertOpen, insert_query);

    try
      let res = self#_get_last_result in
      match (List.length res#get_all_lst) with
	| 1 ->
	  primary_key <- Some (res#getvalue 0 0)
	| _ -> assert false
    with Sql_no_last_result ->
      let err =
	"PgSQL error: RW could not get any result \
        after the file_opened event on file "^f.f_name
      in
      Log.log (err, Error)


  method file_closed closing_date filesize offset created =
    try
      let pkey = self#_get_primary_key in

      let update_query =
	"UPDATE accesses \
         SET CLOSING_DATE = $1, FILESIZE = $2, \
         LAST_KNOWN_OFFSET = $3, CREATED = $4, IN_PROGRESS = '0' \
         WHERE ID = $5"
      in
      let update_query_args =
	[|
	  closing_date;
	  (self#_to_strsql filesize);
	  (self#_to_strsql offset);
	  (match created with
	    | true -> "1"
	    | false -> "0"
	  );
	  pkey
	|]
      in
      self#_query
	~expect:Command_ok
	~disconnect:true
	~args:update_query_args
 	(UpdateClose, update_query)
    with No_primary_key ->
      let err =
	"PgSQL error: could not query the file closing as there is no primary key"
      in
      Log.log (err, Error)


  method private _update_known_offset ?(first=false) ?(last=false) offset =
    try
      let pkey = self#_get_primary_key in

      let (tquery, field) =
	match first, last with
	  | false, true -> (UpdateLastOffset, "LAST_KNOWN_OFFSET")
	  | true, false -> (UpdateFirstOffset, "FIRST_KNOWN_OFFSET")
	  | _ -> assert false
      in

      let update_offset_query =
	"UPDATE accesses \
         SET "^field^" = $1 \
         WHERE ID = $2"
      in
      let update_offset_query_args =
	[|
	  (self#_to_strsql offset);
	  pkey
	|]
      in
      self#_query
	~expect:Command_ok
	~log:false
	~args:update_offset_query_args
	(tquery, update_offset_query)
    with No_primary_key ->
      let err =
	"PgSQL error: could not update the offset as there is no primary key"
      in 
      Log.log (err, Error)


  method private _db_exists =
    let db = self#_get_dbname in

    let q = "SELECT COUNT(*) FROM pg_catalog.pg_database WHERE datname = $1" in
    self#_query
      ~expect:Tuples_ok
      ~nodb:true
      ~args:[|db|]
      (SelectDbExists, q) ;

    try
      let result = self#_get_last_result in
      begin match result#status with
	| Bad_response -> 
	  Log.log ("PgSQL error: bad response when checking db_exists", Error);
	  exit 2
	| Fatal_error ->
	  Log.log ("PgSQL error: fatal error when checking db_exists", Error);
	  exit 2
	| _ -> ()
      end;
      match (List.length result#get_all_lst) with
	| 1 ->
	  if (result#getvalue 0 0) = "1" then
	    true
	  else
	    false
	| _ -> assert false
    with Sql_no_last_result -> false


  method private _index_exists idx =
    let q = "SELECT COUNT(*) FROM pg_class WHERE relname = $1" in    
    self#_query
      ~expect:Tuples_ok
      ~args:[|idx|]
      (SelectIndexExists, q);

    try
      let result = self#_get_last_result in
      match (List.length result#get_all_lst) with
	| 1 ->
	  if (result#getvalue 0 0) = "1" then
	    true
	  else
	    false
	| _ -> assert false
    with Sql_no_last_result -> false


  (* TO DO: get rid of dbname arg *)
  method create_db dbname' =
    match self#_db_exists with
      | true ->
	Log.log (("PgSQL: database '"^dbname'^"' already exists"), Normal_Extra);
	(* Disconnection must be forced because if the database exists
	 * but not the table, the table will be created at the wrong place as the disconnection
	 * would not have been performed *)
	self#disconnect ()
      | false ->
	let q = "CREATE DATABASE "^dbname' in
	self#_query
	  ~expect:Command_ok
	  ~disconnect:true
	  ~nodb:true
	  (CreateDb, q);
	try
	  match (self#_get_last_result)#status with
	    | Bad_response -> 
	      Log.log ("PgSQL error: bad response when creating the db "^dbname', Error);
	      exit 2
	    | Fatal_error ->
	      Log.log ("PgSQL error: fatal error when creating the db "^dbname', Error);
	      exit 2
	    | _ -> Log.log (("PgSQL: database '"^dbname'^"' created"), Normal_Extra)
	with Sql_no_last_result -> exit 2


  method create_table_accesses =
    try
      let create_tbl =
	 "CREATE TABLE IF NOT EXISTS accesses (\
          ID SERIAL PRIMARY KEY,\
          LOGIN varchar(32) NOT NULL,\
          USERNAME varchar(256) NOT NULL,\
          PROGRAM varchar(26) NOT NULL,\
          PROGRAM_PID int NOT NULL,\
          PATH varchar(512) NOT NULL,\
          FILENAME varchar(256) NOT NULL,\
          FILESIZE bigint DEFAULT NULL,\
          FILEDESCRIPTOR int NOT NULL,\
          FIRST_KNOWN_OFFSET bigint DEFAULT NULL,\
          LAST_KNOWN_OFFSET bigint DEFAULT NULL,\
          OPENING_DATE timestamp NOT NULL,\
          CLOSING_DATE timestamp DEFAULT NULL,\
          CREATED smallint NOT NULL,\
          IN_PROGRESS smallint NOT NULL)"
      in

      self#_query
	~expect:Command_ok
	(CreateTable, create_tbl);

      try
	let res = self#_get_last_result in
	begin match res#status with
	  | Bad_response ->
	    Log.log (("PgSQL error: bad response when creating the table. "^(res#error)), Error);
	    exit 2
	  | Fatal_error ->
	    Log.log (("PgSQL error: fatal error when creating the table. "^(res#error)), Error);
	    exit 2
	  | _ ->
	    let created_success =
	      "PgSQL: table created successfully into "^self#_get_connected_dbname
	    in
	    Log.log (created_success, Normal_Extra)
	end;
	
	let idx = "in_progress_idx" in
	let create_progress_idx_query =
	  "CREATE INDEX "^idx^" ON accesses USING btree (IN_PROGRESS)"
	in

	if (self#_index_exists idx) = false then begin
	  self#_query ~expect:Command_ok (CreateIndex, create_progress_idx_query);
	  match (self#_get_last_result)#status with
	    | Bad_response ->
	      Log.log
		(("PgSQL error: bad response when creating the index "^idx^". "^(res#error)), Error);
	      exit 2
	    | Fatal_error ->
	      Log.log
		(("PgSQL error: fatal error when creating the index "^idx^". "^(res#error)), Error);
	      exit 2
	    | _ -> Log.log ("PgSQL: Index created successfully", Normal_Extra)
	end;
      with Sql_no_last_result -> exit 2

    with Sql_not_connected ->
      Log.log ("PgSQL error: Object not connected, cannot create table or index", Error);
      exit 2

  method reset_in_progress =
    self#_query
      ~expect:Command_ok
      ~disconnect:true
      (UpdateResetProgress, reset_accesses_query)


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
         WHERE ID = $1"
      in
      let switch_on_created_query_arg = [|pkey|] in
      self#_query
	~expect:Command_ok
	~args:switch_on_created_query_arg
 	(UpdateCreated, switch_on_created_query)
    with No_primary_key ->
      let err =
	"PgSQL error: could not switch on 'created' as there is no primary key"
      in 
      Log.log (err, Error)

end;;
