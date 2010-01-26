open Unix

let (tor,tow) = Unix.pipe()

let date () =
  let date = (localtime(time())) in

  (* Starts in 1900 *)
  let year = string_of_int (1900 + date.tm_year) in
    (* month = 0..11 *)
  let month = string_of_int (1 + date.tm_mon) in
  let day = string_of_int date.tm_mday in
  let hour = string_of_int date.tm_hour in
  let minute = string_of_int date.tm_min in
  let second = string_of_int date.tm_sec in
  
  (* 2008-01-19 16:21:00 *)
  Printf.sprintf "%s-%s-%s %s:%s:%s" year month day hour minute second
;;


let log txt =
  let to_log = Printf.sprintf "%s\t%s" (date()) txt in
  ignore (Unix.system ("echo \""^to_log^"\" >> log.txt"))
;;



let notify txt =
  Printf.printf "Notify: %s\n" txt;  
  (* Send in the pipe for the server to send to the clients *)
  ignore (Unix.write tow txt 0 (String.length txt))
;;
