open Types
open Types_conf

exception Sql_not_connected
exception Sql_no_last_result
exception No_primary_key

type sql_tquery = 
  | CreateDb
  | CreateTable
  | CreateIndex
  | InsertOpen
  | UpdateFirstOffset
  | UpdateFirstOffset_Null (* workaround MySQL prep statement bug *)
  | UpdateLastOffset
  | UpdateLastOffset_Null (* workaround MySQL prep statement bug *)
  | UpdateCreated
  | UpdateClose
  | UpdateResetProgress
  | SelectDbExists
  | SelectIndexExists

type a_statement = Mysqlst of Mysql.Prepared.stmt | Postgresqlst

class virtual abstract_sql =
object(self)

  val mutable cid = None
  val mutable primary_key = None
  val mutable last_result = None
  val mutable stmt_db_exists = ref None
  val mutable stmt_idx_exists = ref None
  val mutable stmt_create_idx = ref None
  val mutable stmt_create_db = ref None
  val mutable stmt_create_table = ref None
  val mutable stmt_insert_open = ref None
  val mutable stmt_update_first_offset = ref None
  val mutable stmt_update_first_offset_null = ref None
  val mutable stmt_update_last_offset = ref None
  val mutable stmt_update_last_offset_null = ref None
  val mutable stmt_update_created = ref None
  val mutable stmt_update_close = ref None
  val mutable stmt_update_reset_progress = ref None
  val reset_accesses_query =
    "UPDATE accesses SET IN_PROGRESS = '0' WHERE IN_PROGRESS = '1'"

  (* Lock proven to be useful because of the offset_thread using the very same object
   * There was a concurrency between the 2 threads.
   * One disconnected from postgres while the other thread thought to be still connected
   * while executing the query.
   * It resulted in the error: Failure("Postgresql.check_null: connection already finished")
   * Lock also added to mysqldb
   *)
  val lock' = Mutex.create ()

  method virtual connect_without_db : unit
  method virtual disconnect : ?log:bool -> unit -> unit
  method virtual file_opened : Types.f_file -> bool -> string -> int64 option -> unit
  method virtual file_closed : string -> int64 option -> int64 option -> bool -> unit
  method virtual first_known_offset : int64 option -> unit
  method virtual last_known_offset : int64 option -> unit
  method virtual switch_on_created : unit
  method virtual reset_in_progress : unit
  method virtual create_db : string -> unit
  method virtual create_table_accesses : unit

  method private _get_cid =
    match cid with
      | None -> raise Sql_not_connected
      | Some cid' -> cid'


  method private _get_primary_key =
    match primary_key with
      | None -> raise No_primary_key
      | Some pkey -> pkey


  method private _get_last_result =
    match last_result with
      | None -> raise Sql_no_last_result
      | Some last_r -> last_r


  method private _to_strsql int64opt =
    match int64opt with
      | Some var_int64 -> Int64.to_string var_int64
      | None ->
	match Config.cfg#get_sql_rdbms with
	  | MySQL -> "NULL"
	  | PostgreSQL -> Postgresql.null


  method private _args_list_to_string res arg_l =
    let rec r_args_list_to_string r a_l =
      match a_l with
	| [] -> r
	| arg :: [] -> r^arg
	| arg :: t -> r_args_list_to_string (r^arg^", ") t
    in
    r_args_list_to_string res arg_l


  method private _get_lock = lock'


  method is_connected =
    match cid with
      | None -> false
      | Some _ -> true


  method private _cleanup_prepare_stmts =
    let cleanup_ctr = ref 0 in
    
    let cleanup statement_var =
      statement_var := None;
      incr cleanup_ctr;
    in

    List.iter (fun statement_var ->
      match !statement_var with
	| None -> ()
	| Some (Mysqlst st') ->
	  Mysql.Prepared.close st';
	  cleanup statement_var
	| Some Postgresqlst -> 
	  cleanup statement_var
    )
      [stmt_db_exists;
       stmt_idx_exists ;
       stmt_create_idx ;
       stmt_create_db ;
       stmt_create_table ;
       stmt_insert_open ;
       stmt_update_first_offset ;
       stmt_update_first_offset_null ;
       stmt_update_last_offset ;
       stmt_update_last_offset_null ;
       stmt_update_created ;
       stmt_update_close ;
       stmt_update_reset_progress
      ];

    Log.log
      ((string_of_int !cleanup_ctr)^" prepared statement(s) closed", Normal_Extra)

end;;
