val mutex_ht : Mutex.t

(* string = date
 * bool = is the first offset known (meaning, is the offset a None) ?
 * int64 option = offset
 * int64 = SQL primary key
*)
val ht : (Inotify.wd * Types.f_file, (string * (bool * int64 option) * int64)) Hashtbl.t
