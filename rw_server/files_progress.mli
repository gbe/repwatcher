val mutex_ht : Mutex.t

(*
 * int = times the file has been opened
 * string = date
 * int64 = filesize
 *
 * bool = is the first offset known (meaning, is the offset a None) ?
 * int64 option = offset
 * int = times the offset couldn't be retrieved (error_counter)
 *
 * int64 = SQL primary key
*)
val ht : (Inotify.wd * Types.f_file, (int * string * int64 option * (bool * int64 option * int) * int64)) Hashtbl.t
