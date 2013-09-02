exception Sql_not_connected
exception Sql_no_last_result
exception No_primary_key

class virtual abstract_sql =
object(self)

  val mutable cid = None
  val mutable primary_key = None
  val mutable last_result = None

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
  method virtual file_opened : Types.f_file -> bool -> string -> int64 option -> int64 option -> unit
  method virtual file_closed : string -> int64 option -> int64 option -> unit
  method virtual first_known_offset : int64 option -> unit
  method virtual last_known_offset : int64 option -> unit
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

  method private get_lock = lock'

  method is_connected =
    match cid with
      | None -> false
      | Some _ -> true

end;;
