open Postgresql
open Printf

exception Pgsql_not_connected;;
exception No_PG_last_result;;

class pgsql =
object(self)

  val mutable cid = None
  val mutable host = None
  val mutable port = None
  val mutable dbname = None
  val mutable user = None
  val mutable pwd = None
  val mutable requiressl = None

  val mutable last_result = None

  initializer
    host <- Some "192.168.69.22";
    dbname <- Some "huhu42";
    user <- Some "postgres" ;
    pwd <- Some "postgres" ;

  method private _get_last_result =
    match last_result with
      | None -> raise No_PG_last_result
      | Some last_r -> last_r


  method private _get_cid =
    match cid with
      | None -> raise Pgsql_not_connected
      | Some cid' -> cid'


  method is_connected =
    match cid with
      | None -> false
      | Some _ -> true


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
      | Error e -> prerr_endline (string_of_error e)
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
      | Pgsql_not_connected ->
	prerr_endline "Object not connected to Postgresql, cannot disconnect"
      | Error e -> prerr_endline (string_of_error e)
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
      | Pgsql_not_connected ->
	prerr_endline "Object not connected to Postgresql, cannot query";
      | Error e -> prerr_endline (string_of_error e)
      | e -> prerr_endline (Printexc.to_string e)



  method file_opened q =
    self#_query ~expect:Tuples_ok q


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
    with No_PG_last_result -> false


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
    with No_PG_last_result -> false


  method create_db =
    let dbname' = self#_get_dbname in

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

    with Pgsql_not_connected -> prerr_endline "Object not connected to Postgresql, cannot create table"
end;;


(*
let _ =
  let p1 = new pgsql in
  let p2 = new pgsql in
  p1#create_db;
  p2#create_table_accesses;

  let q_insert = Printf.sprintf "insert into accesses (login, program, program_pid, path, filename, filedescriptor, opening_date, created, in_progress) values ('greg', 'monprog', 3, '/mon path', 'mon fichier.txt', 42, '2013-07-18 19:07:00', 1, 1) RETURNING ID" in

  p2#file_opened q_insert ;

  let res = p2#_get_last_result in

  match (List.length res#get_all_lst) with
    | 1 ->
      print_endline ("résultat: "^(res#getvalue 0 0))
    | _ -> assert false
  
with
    | Error e -> prerr_endline (string_of_error e)
    | e -> prerr_endline (Printexc.to_string e)
*)
