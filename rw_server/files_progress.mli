(* The string here is the date and int64 the offset when the file has been accessed *)
val ht : (Inotify.wd * Types.f_file, (string * int64)) Hashtbl.t
