val mutex_ht : Mutex.t

(* string = date
 * int64 option = last offset known
 * int64 = SQL primary key
*)
val ht : (Inotify.wd * Types.f_file, (string * int64 option * int64)) Hashtbl.t
