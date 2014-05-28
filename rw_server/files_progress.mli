val mutex_ht : Mutex.t

(*
 * Date.date = opening date
 *
 * int64 option = filesize
 * bool = filesize_checked_again
 *
 * int64 option = first known offset
 * int64 option = last known offset
 * int = times the offset couldn't be retrieved (error_counter)
 *
 * Sqldb.sqldb is an SQL object (could be either Mysql or Postgresql)
 * bool = created (true) or not (false)
*)
val ht : (Inotify.wd * Types.f_file, 
	  (Date.date
	   * (int64 option * bool)
	   * (int64 option * int64 option * int) 
	   * Sqldb.sqldb option
	   * bool)
) Hashtbl.t
