open Lexing
open Format
open Types
open Types_conf
open Unix

exception Config_error ;;
exception Email_not_configured ;;
exception Process_identity_not_configured ;;
exception SQL_not_configured ;;

let usage = "usage: rw_server [-f Configuration file path]" ;;

class config =
object(self)

  val mutable conf = None

(* localize the error and give the line and column *)
  method private _localization (pos,e) config_file =
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
       * main.ml. The function tries to clean up the RDBMS and tries to
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
	self#_localization (lexeme_start_p lb, lexeme_end_p lb) cfg_file;
	eprintf "lexical error in the configuration file %s: %s\n@." cfg_file s;
	exit 1
      | Parsing.Parse_error ->
	self#_localization (lexeme_start_p lb, lexeme_end_p lb) cfg_file;
	eprintf "syntax error in the configuration file %s\n@." cfg_file;
	exit 1
    end;

    close_in c;


  method get_email =
    let c =
      try
	(self#get).c_email 
      with Config_error ->
	let err = "Cannot retrieve email configuration since the config file has not been parsed" in
	Log.log (err, Error);
	exit 1
    in
    match c with
      | None -> raise Email_not_configured
      | Some email_conf -> email_conf

  method is_email_activated =
   try
     ignore (self#get_email);
     true
   with Email_not_configured -> false


  method set_email_disabled =
    conf <- Some { self#get with c_email = None }

  method get_sql =
      let c =
	try
	  (self#get).c_sql
	with Config_error ->
	  let err = "Cannot retrieve SQL configuration since the config file has not been parsed" in
	 Log.log (err, Error);
	  exit 1
      in
      match c with
	| None -> raise SQL_not_configured
	| Some sql_conf -> sql_conf


  method is_sql_activated =
       try
     ignore (self#get_sql);
     true
   with SQL_not_configured -> false

  method get_sql_rdbms =
    try
      (self#get_sql).sql_rdbms
    with SQL_not_configured ->
      Log.log ("Cannot get the RDBMS since SQL is not configured", Error);
      exit 1


  method get_process_identity =
    let c =
      try
	(self#get).c_process_identity
      with Config_error ->
	let err = "Cannot retrieve process identity configuration since the config file has not been parsed" in
	Log.log (err, Error);
	exit 1
    in
    match c with
      | None -> raise Process_identity_not_configured
      | Some process_id_conf -> process_id_conf

  method get_log_verbosity =
    try
      (self#get).c_log
    with Config_error ->
      let err = "Cannot retrieve the log verbosity since the config file has not been parsed" in
      Log.log (err, Error);
      exit 1
end ;;
		  
let cfg = new config ;;
