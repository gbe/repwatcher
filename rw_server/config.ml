open Lexing
open Format

let conf = ref None;;

(* localize the error and give the line and column *)
let localization (pos,e) config_file =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  let lc = e.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, character %d-%d:\n" config_file l c lc
;;

let get () =
  match !conf with
    | None -> assert false
    | Some c -> c
;;

let parse config_file =
  let c = open_in config_file in
  let lb = Lexing.from_channel c in
    try
      let res = Parser.configuration Lexer.nexttoken lb in
      conf := Some res ;
      close_in c;
      res

    with
      | Lexer.Lexing_error s -> 
	  localization (lexeme_start_p lb, lexeme_end_p lb) config_file;
	  eprintf "lexical error in the configuration file %s: %s\n@." config_file s;
	  exit 1
      | Parsing.Parse_error ->
	  localization (lexeme_start_p lb, lexeme_end_p lb) config_file;
	  eprintf "syntax error in the configuration file %s\n@." config_file;
	  exit 1
;; 
		  
