{
  open Lexing
  open Parser
   
  exception Lexing_error of string
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let space = [' ' '\t']

let comment = '#'([^'\n'])*'\n'

let txt = '\"' [^'\"']* '\"'


rule nexttoken = parse
  | comment                            { newline lexbuf; nexttoken lexbuf }
  | '='                                { EQUAL }
  | ';'                                { PVIRGULE }
  | "directories"                      { DIRECTORIES }
  | "ignore_directories"               { IGNORE_DIRECTORIES }
  | "ignore_users"                     { IGNORE_USERS }
  | "specified_programs"               { SPECIFIED_PROGRAMS }
  | "unwanted_programs"                { UNWANTED_PROGRAMS }
  | "process_identity"                 { PROCESS_IDENTITY }
  | "mysql_login"                      { MYSQL_LOGIN }
  | "mysql_pswd"                       { MYSQL_PSWD }
  | "mysql_host"                       { MYSQL_HOST }
  | "mysql_port"                       { MYSQL_PORT }
  | "mysql_dbname"                     { MYSQL_DBNAME }
  | "notify_locally"                   { NOTIFY_LOCALLY }
  | "notify_remotely"                  { NOTIFY_REMOTELY }
  | "notify_parent_folders"            { PARENT_FOLDERS }
  | "server_ca_path"                   { SERVER_CA_PATH }
  | "server_cert_path"                 { SERVER_CERT_PATH }
  | "server_key_path"                  { SERVER_KEY_PATH }
  | "server_key_pwd"                   { SERVER_KEY_PWD }
  | "server_port"                      { SERVER_PORT }
  | "server_process_identity"          { SERVER_PROCESS_IDENTITY }
  | "server_process_chroot"            { SERVER_PROCESS_CHROOT }
  | "log_level"                        { LOG_LEVEL }
  | "log_directory"                    { LOG_DIRECTORY }
  | eof                                { EOF }
  | '\n'                               { newline lexbuf; nexttoken lexbuf }
  | space+                             { nexttoken lexbuf }
  | txt                                { let s = lexeme lexbuf in TXT (String.sub s 1 ((String.length s)-2) )}
  | _                                  { raise (Lexing_error (lexeme lexbuf)) }
