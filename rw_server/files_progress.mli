type in_progress_t = {
  ip_common : Types.common_t ;
  ip_filesize_checked_again : bool ;
  ip_offset_retrieval_errors : int ref;
  ip_sql_connection : Sqldb.sqldb option ;
}

val mutex_ht : Mutex.t
val ht : (Inotify.wd * Types.f_file, in_progress_t) Hashtbl.t
val htbuffer : (Inotify.wd * Types.f_file, in_progress_t) Hashtbl.t
val htadd : (Inotify.wd * Types.f_file) -> in_progress_t -> unit
val htremove : (Inotify.wd * Types.f_file) -> in_progress_t -> unit
val htreplace : (Inotify.wd * Types.f_file) -> in_progress_t -> unit
val remove_closed_files : unit -> unit
