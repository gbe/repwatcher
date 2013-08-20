open Postgresql
open Types

exception Sql_not_connected;;
exception Sql_no_last_result;;
exception No_primary_key;;

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
    host <- Some "192.168.69.22";
    dbname <- Some "repwatcher";
    user <- Some "postgres" ;
    pwd <- Some "postgres" ;


  method private _to_strsql int64opt =
    match int64opt with
      | None -> "NULL"
      | Some var_int64 -> Int64.to_string var_int64


  method private _get_dbname =
    match dbname with
      | None -> assert false
      | Some dbname' -> dbname'


  method private _connect ?(nodb=false) () =

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
      field "dbname" (match nodb with false -> dbname | true -> None);
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
      print_endline "Connected";    
      cid <- Some c
    with
      | Postgresql.Error e -> prerr_endline (string_of_error e)
      | e -> prerr_endline (Printexc.to_string e)


  method connect_without_db =
    self#_connect ~nodb:true ()


  method disconnect ?(log=true) () =
    try
      (self#_get_cid)#finish ;
      cid <- None;

      if log then
	print_endline "Disconnected"

    with
      | Sql_not_connected ->
	prerr_endline "Object not connected to Postgresql, cannot disconnect"
      | Postgresql.Error e -> prerr_endline (string_of_error e)
      | e -> prerr_endline (Printexc.to_string e)



  method private _query ~expect ?(log=true) ?(nodb=false) q =
    if log then
      print_endline ("Next SQL query to compute:\n"^q^"\n");

    if self#is_connected = false then begin
      match nodb with
	| false -> self#_connect () 
	| true -> self#connect_without_db
    end;
	  
    try
      last_result <- Some ((self#_get_cid)#exec ~expect:[expect] q) ;
      self#disconnect ()

    with 
      | Sql_not_connected ->
	prerr_endline "Object not connected to Postgresql, cannot query";
      | Postgresql.Error e -> prerr_endline (string_of_error e)
      | e -> prerr_endline (Printexc.to_string e)



  method file_opened f s_created creation_date filesize offset =
    let username =
      match Txt_operations.name f.f_login with
	| None -> "NULL"
	| Some username -> username
    in
    let insert_query =
      Printf.sprintf "INSERT INTO accesses \
         (login, username, program, program_pid, path, filename, filesize, \
         filedescriptor, first_known_offset, opening_date, created, in_progress) \
	  VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '1') \
          RETURNING ID"
	f.f_login
	username
	f.f_program
	(string_of_int (Fdinfo.int_of_pid f.f_program_pid))
	f.f_path
	f.f_name
	(self#_to_strsql filesize)
	(string_of_int (Fdinfo.int_of_fd f.f_descriptor))
	(self#_to_strsql offset)
	creation_date
	(match s_created with
	  | true -> "1"
	  | false -> "0"
	)
    in
    self#_query ~expect:Tuples_ok insert_query;

    try
      let res = self#_get_last_result in     
      match (List.length res#get_all_lst) with
	| 1 ->
	  primary_key <- Some (res#getvalue 0 0)
	| _ -> assert false
    with Sql_no_last_result -> prerr_endline "file_opened, no result ? Impossible"  


  method file_closed closing_date filesize offset =
    try
      let pkey = self#_get_primary_key in

      let update_query =
	Printf.sprintf "UPDATE accesses \
        	  SET CLOSING_DATE = '%s', FILESIZE = '%s', \
                  LAST_KNOWN_OFFSET = '%s', IN_PROGRESS = '0' \
        	  WHERE ID = '%s'"
	  closing_date
	  (self#_to_strsql filesize)
	  (self#_to_strsql offset)
	  pkey
      in
      self#_query ~expect:Command_ok update_query
    with No_primary_key ->
      prerr_endline "file closed: no prim key"


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
        	  SET %s = '%s' \
	          WHERE ID = '%s'"
	  field
	  (self#_to_strsql offset)
	  pkey
      in
      self#_query ~expect:Command_ok update_offset_query
    with No_primary_key ->
      prerr_endline "update offset: no prim key"


  method reset_in_progress =
    let reset_accesses =
      "UPDATE accesses SET IN_PROGRESS = '0' WHERE IN_PROGRESS = '1'"
    in
    self#_query ~expect:Command_ok reset_accesses


  method first_known_offset offset =
    self#_update_known_offset ~first:true offset


  method last_known_offset offset =
    self#_update_known_offset ~last:true offset


  method private _db_exists =
    let db = self#_get_dbname in

    let q = "select count(*) from pg_catalog.pg_database where datname = '"^db^"'" in
    self#_query ~expect:Tuples_ok ~nodb:true q ;

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


  method private _index_exists idx =
    let q = "select count(*) from pg_class where relname='"^idx^"'" in
    
    self#_query ~expect:Tuples_ok q;

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
(*    let dbname' = self#_get_dbname in*)

    if self#_db_exists then
      print_endline "Already exists"
    else begin
      let q = "CREATE DATABASE "^dbname' in
      self#_query ~expect:Command_ok ~nodb:true q;
      print_endline (dbname'^" created")
    end


  method create_table_accesses =
    try
      let create_tbl =
	Printf.sprintf "CREATE TABLE IF NOT EXISTS accesses (\
          ID SERIAL PRIMARY KEY,\
          LOGIN varchar(20) NOT NULL,\
          USERNAME varchar(256) DEFAULT NULL,\
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

      self#_query ~expect:Command_ok create_tbl;
      
      let idx = "in_progress_idx" in

      let create_progress_idx =
	Printf.sprintf "CREATE INDEX "^idx^" ON accesses USING btree (IN_PROGRESS)"
      in

      if (self#_index_exists idx) = false then
	self#_query ~expect:Command_ok create_progress_idx

    with Sql_not_connected -> prerr_endline "Object not connected to Postgresql, cannot create table"
end;;
