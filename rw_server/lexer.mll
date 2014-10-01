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
  | "sql_rdbms"                        { SQL_RDBMS }
  | "sql_login"                        { SQL_LOGIN }
  | "sql_pswd"                         { SQL_PSWD }
  | "sql_host"                         { SQL_HOST }
  | "sql_port"                         { SQL_PORT }
  | "sql_dbname"                       { SQL_DBNAME }
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
  | "email_open"                       { EMAIL_OPEN }
  | "email_close"                      { EMAIL_CLOSE }
  | "email_buffer_frequency"           { EMAIL_BUFFER_FREQUENCY }
  | "email_sender_name"                { EMAIL_SENDER_NAME }
  | "email_sender_address"             { EMAIL_SENDER_ADDRESS }
  | "email_recipients"                 { EMAIL_RECIPIENTS }
  | "smtp_host"                        { SMTP_HOST }
  | "smtp_port"                        { SMTP_PORT }
  | "smtp_username"                    { SMTP_USERNAME }
  | "smtp_passwd"                      { SMTP_PASSWD }
  | "smtp_ssl"                         { SMTP_SSL }
  | "log_level"                        { LOG_LEVEL }
  | eof                                { EOF }
  | '\n'                               { newline lexbuf; nexttoken lexbuf }
  | space+                             { nexttoken lexbuf }
  | txt                                { let s = lexeme lexbuf in TXT (String.sub s 1 ((String.length s)-2) )}
  | _                                  { raise (Lexing_error (lexeme lexbuf)) }
