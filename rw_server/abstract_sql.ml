exception No_primary_key

class virtual abstract_sql =
object(self)

  val mutable primary_key = None

  method virtual connect_without_db : unit
  method virtual is_connected : bool
  method virtual disconnect : ?log:bool -> unit -> unit
  method virtual file_opened : Types.f_file -> bool -> string -> int64 option -> int64 option -> unit
  method virtual file_closed : string -> int64 option -> int64 option -> unit
  method virtual first_known_offset : int64 option -> unit
  method virtual last_known_offset : int64 option -> unit
  method virtual reset_in_progress : unit
  method virtual create_db : string -> unit
  method virtual create_table_accesses : unit

  method private _get_primary_key =
    match primary_key with
      | None -> raise No_primary_key
      | Some pkey -> pkey
end;;
