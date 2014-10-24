open Types

type in_progress_t = {
  ip_common : Types.common_t ;
  ip_filesize_checked_again : bool ;
  ip_offset_retrieval_errors : int ref;
  ip_sql_connection : Sqldb.sqldb option ;
}

let mutex_ht = Mutex.create () ;;
let ht = Hashtbl.create 41;;

let remove_closed_files () =
  Printf.printf "HT size avant: %d\n" (Hashtbl.length ht);
  Pervasives.flush Pervasives.stdout;

  Hashtbl.iter (fun key in_progress ->
    match in_progress.ip_common.c_closing_date with
    | None -> ()
    | Some _ -> Hashtbl.remove ht key
  ) ht;

  Printf.printf "HT size apres: %d\n" (Hashtbl.length ht);
  Pervasives.flush Pervasives.stdout;

;;
