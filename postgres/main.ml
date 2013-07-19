open Postgresql
open Printf

exception Pgsql_Not_Connected;;

class pgsql =
object(self)

  val mutable cid = None;
  val mutable host = None;
  val mutable port = None;
  val mutable dbname = None;
  val mutable user = None;
  val mutable pwd = None;
  val mutable requiressl = None;

  initializer
    host <- Some "192.168.69.22";
    dbname <- Some "huhu42";
    user <- Some "postgres" ;
    pwd <- Some "postgres" ;

  method private getCid =
    match cid with
      | None -> raise Pgsql_Not_Connected
      | Some cid' -> cid'

  method private getDBName =
    match dbname with
      | None -> assert false
      | Some dbname' -> dbname'


  method connect ?(nodb=false) () =

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

    let c =
      new connection ~conninfo:conninfo ()
    in
    print_endline "Connected";    
    cid <- Some c

  method disconnect =
    (self#getCid)#finish ;
    print_endline "Disconnected"

  method private query ~expect q =
    (self#getCid)#exec ~expect:[expect] q

  method insert q =
    self#query ~expect:Tuples_ok q;

  method private db_exists =
    let db = self#getDBName in

    let q = "select count(*) from pg_catalog.pg_database where datname = '"^db^"'" in
    let result = self#query ~expect:Tuples_ok q in
    match (List.length result#get_all_lst) with
      | 1 ->
	if (result#getvalue 0 0) = "1" then
	  true
	else
	  false
      | _ -> assert false


  method private index_exists idx =
    let q = "select count(*) from pg_class where relname='"^idx^"'" in
    
    let result = self#query ~expect:Tuples_ok q in
    match (List.length result#get_all_lst) with
      | 1 ->
	if (result#getvalue 0 0) = "1" then
	  true
	else
	  false
      | _ -> assert false


  method create_db =
    let dbname' = self#getDBName in

    if self#db_exists then
      print_endline "Already exists"
    else begin
      let q = "CREATE DATABASE "^dbname' in
      ignore (self#query ~expect:Command_ok q);
      print_endline (dbname'^" created")
    end;
   (* 2008-01-19 16:21:00 *)
  method create_table =
    (* to prevent a programming error for which the table would be created in another db *)
    if (self#getCid)#db = self#getDBName then
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

      ignore (self#query ~expect:Command_ok create_tbl);

      let idx = "in_progress_idx" in
      let create_progress_idx =
	Printf.sprintf "CREATE INDEX "^idx^" ON accesses USING btree (IN_PROGRESS)"
      in
      begin if (self#index_exists idx) = false then
	  ignore (self#query ~expect:Command_ok create_progress_idx)
      end;
    else
      prerr_endline "Error pas pareil dbname"  
end;;



let _ =

  let p = new pgsql in
  try    
    p#connect ~nodb:true ();
    p#create_db;
    p#disconnect;

    p#connect ();
    ignore (p#create_table);

    let q_insert = Printf.sprintf "insert into accesses (login, program, program_pid, path, filename, filedescriptor, opening_date, created, in_progress) values ('greg', 'monprog', 3, '/mon path', 'mon fichier.txt', 42, '2013-07-18 19:07:00', 1, 1) RETURNING ID" in

    let res = p#insert q_insert in
    begin match (List.length res#get_all_lst) with
      | 1 ->
	print_endline ("résultat: "^(res#getvalue 0 0))
      | _ -> assert false
    end;

    p#disconnect;
  with
    | Pgsql_Not_Connected -> prerr_endline "postgres pas connected"
    | Error e -> prerr_endline (string_of_error e)
    | e -> prerr_endline (Printexc.to_string e)
;;
