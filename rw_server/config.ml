open Lexing
open Format

let conf = ref None;;
exception Config_error ;;



(* localize the error and give the line and column *)
let localization (pos,e) config_file =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  let lc = e.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, character %d-%d:\n" config_file l c lc
;;

let get () =
  match !conf with
    | None ->
      (* This exception should never be raised but there is a case
       * when it can be raised. When there is a problem parsing the
       * configuration file, it raises a Parse_error. The program
       * must exit right away but clean_exit() is then executed from
       * main.ml. The function tries to clean up the sgbd and tries to
       * connect using the info from the configuration which doesn't
       * exist due to the Parse_error. *)
      raise Config_error

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
		  
