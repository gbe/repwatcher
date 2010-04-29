open Unix

let (tor,tow) = Unix.pipe()

let date () =
  let date = (localtime(time())) in

  (* Starts in 1900 *)
  (* month = 0..11 *)

  (* 2008-01-19 16:21:00 *)
  Printf.sprintf "%d-%d-%d %d:%d:%d" (1900 + date.tm_year) (1 + date.tm_mon) date.tm_mday date.tm_hour date.tm_min date.tm_sec
;;


let log txt =
  let to_log = Printf.sprintf "%s\t%s" (date()) txt in
  Printf.printf "LOG: %s\n" to_log   ;
  Pervasives.flush Pervasives.stdout ;
  ignore (Unix.system ("echo \""^to_log^"\" >> log.txt"))
;;



let notify txt =
  Printf.printf "Notify: %s\n" txt;  
  (* Send in the pipe for the server to send to the clients *)
  ignore (Unix.write tow txt 0 (String.length txt))
;;
