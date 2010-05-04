open Unix
open Ast
open Ast_conf

let (tor,tow) = Unix.pipe()

let log txt =
  let to_log = Printf.sprintf "%s\t%s" (Date.date()) txt in
  Printf.printf "LOG: %s\n" to_log   ;
  Pervasives.flush Pervasives.stdout ;
  ignore (Unix.system ("echo \""^to_log^"\" >> log.txt"))
;;



let notify txt =
  Printf.printf "Notify: %s\n" txt;  
  
  let conf = Config.get() in
    
    if conf.c_notify_loc then
      ignore (system ("notify-send -i /usr/share/pixmaps/nobody.png Repwatcher '"^txt^"'"))     
    ;
    if conf.c_notify_rem then
      (* Send in the pipe for the server to send to the clients *)
      ignore (Unix.write tow txt 0 (String.length txt))
;;

module Report =
struct
	(* tor is now viewable from the outside (ssl_server) *)
	let tor = tor ;;

	let report rep = match rep with
		| Notify t -> notify t
		| Log 	 t -> log t
	;;
end;;
