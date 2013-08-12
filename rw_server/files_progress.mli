open Sql_report ;;

val mutex_ht : Mutex.t

(*
 * string = date
 * int64 = filesize
 *
 * bool = is the first offset known (meaning, is the offset a None) ?
 * int64 option = offset
 * int = times the offset couldn't be retrieved (error_counter)
 *
 * sql_mid = SQL object (either Mysql or Postgresql)
 * bool = created (true) or not (false)
*)
val ht : (Inotify.wd * Types.f_file, 
	  (string 
	   * int64 option 
	   * (bool * int64 option * int) 
	   * dbkind option
	   * bool)
) Hashtbl.t
