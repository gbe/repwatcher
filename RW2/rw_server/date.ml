open Unix

let date () =
let date = (localtime(time())) in
  (* Starts in 1900 *)
  (* month = 0..11 *)

  (* 2008-01-19 16:21:00 *)
  Printf.sprintf "%02d-%02d-%02d %02d:%02d:%02d" (1900 + date.tm_year) (1 + date.tm_mon)     date.tm_mday date.tm_hour date.tm_min date.tm_sec
;;
