open Lexing
open Format
open Types_conf
open Unix

exception Config_error ;;
let usage = "usage: rw_server [-f Configuration file path]" ;;

class config () =
object(self)

  val mutable conf = None

(* localize the error and give the line and column *)
  method private localization (pos,e) config_file =
    let l = pos.pos_lnum in
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    let lc = e.pos_cnum - pos.pos_bol + 1 in
    eprintf "File \"%s\", line %d, character %d-%d:\n" config_file l c lc

  method get =
    match conf with
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

  method parse cfg_file =
    begin try
    (* Check if the file exists and if the process can read it *)
      Unix.access cfg_file [F_OK ; R_OK];
    with
      | Unix_error (error,_,file) ->
	let err =
	  Printf.sprintf "%s: %s.\n\n%s\n" file (error_message error) usage
	in
      (* Do not log because we can't know what
	 the user decided to do about his log policy
	 and where he chose to log
      *)
	failwith err
    end;

    let c = open_in cfg_file in
    let lb = Lexing.from_channel c in

    begin try
      conf <- Some (Parser.configuration Lexer.nexttoken lb)
    with
      | Lexer.Lexing_error s -> 
	self#localization (lexeme_start_p lb, lexeme_end_p lb) cfg_file;
	eprintf "lexical error in the configuration file %s: %s\n@." cfg_file s;
	exit 1
      | Parsing.Parse_error ->
	self#localization (lexeme_start_p lb, lexeme_end_p lb) cfg_file;
	eprintf "syntax error in the configuration file %s\n@." cfg_file;
	exit 1
    end;

    close_in c;


  method set_email_disabled =
    conf <- Some { self#get with c_email = None }

end ;;
		  
let cfg = new config ();;
