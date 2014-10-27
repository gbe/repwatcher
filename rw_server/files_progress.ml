open Types

type in_progress_t = {
  ip_common : Types.common_t ;
  ip_filesize_checked_again : bool ;
  ip_offset_retrieval_errors : int ref;
  ip_sql_connection : Sqldb.sqldb option ;
}

let htbuffer = Hashtbl.create 71;;

let mutex_ht = Mutex.create () ;;
let ht = Hashtbl.create 41;;

let htadd key value =
  Hashtbl.add ht key value;
  Hashtbl.add htbuffer key value;
;;

let htremove key in_progress =
  Hashtbl.remove ht key;
  Hashtbl.replace htbuffer key in_progress
;;

let htreplace key newvalue =
  Hashtbl.replace ht key newvalue
;;

let remove_closed_files () =
  Printf.printf "HT size avant: %d\n" (Hashtbl.length htbuffer);
  Pervasives.flush Pervasives.stdout;

  Hashtbl.iter (fun key in_progress ->
    match in_progress.ip_common.c_closing_date with
    | None -> ()
    | Some _ -> Hashtbl.remove htbuffer key
  ) htbuffer;

  Printf.printf "HT size apres: %d\n" (Hashtbl.length htbuffer);
  Pervasives.flush Pervasives.stdout;

;;
