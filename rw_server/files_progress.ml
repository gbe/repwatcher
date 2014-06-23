type in_progress_t = {
  ip_common : Types.common_t ;
  ip_filesize_checked_again : bool ;
  ip_offset_retrieval_errors : int ref;
  ip_sql_connection : Sqldb.sqldb option ;
}

let mutex_ht = Mutex.create () ;;
let ht = Hashtbl.create 41;;
