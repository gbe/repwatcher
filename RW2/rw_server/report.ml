open Unix
open Ast
open Ast_conf

let l_reg_char_encoded =

(* &amp; needs to be the last one of the list.
 * Otherwise, &gt; (for example) will be changed in &amp;gt;
 *)
  let l_char = [ (">", "&gt;") ;
		 ("<", "&lt;") ;
		 ("'", "&apos;") ;
		 ((Char.escaped '"'), "&quot;");
		 ("&", "&amp;")
	       ]
  in
  List.map (fun (char, char_encoded) ->
    ((Str.regexp char), char_encoded)
	   ) l_char
;;


let (tor,tow) = Unix.pipe();;	
    	
let log txt =
  let to_log = Printf.sprintf "%s\t%s" (Date.date()) txt in
  Printf.printf "LOG: %s\n" to_log   ;
  Pervasives.flush Pervasives.stdout ;
  ignore (Unix.system ("echo \""^to_log^"\" >> log.txt"))
;;
    
    
    
let notify txt =
  Printf.printf "Notify: %s\n" txt;  
  
  let conf = Config.get() in

  let txt_escaped =
    List.fold_right (fun (reg, char_encoded) txt' ->
      Str.global_replace reg char_encoded txt'
		 ) l_reg_char_encoded txt
  in

  if conf.c_notify_loc then
    begin
      let call = Printf.sprintf "notify-send -i nobody Repwatcher \"%s\"" txt_escaped in
      ignore (system call)
    end
      ;
  if conf.c_notify_rem then
    (* Send in the pipe for the server to send to the clients *)
    ignore (Unix.write tow txt_escaped 0 (String.length txt_escaped))
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
