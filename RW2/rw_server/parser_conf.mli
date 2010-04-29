type token =
  | DQUOTE
  | EQUAL
  | PVIRGULE
  | DIRECTORIES
  | MODE
  | SPEC
  | UNWANTED
  | SQL_LOGIN
  | SQL_PSWD
  | SQL_HOST
  | SQL_PORT
  | SQL_DBNAME
  | TXT of (string)
  | DIGITS of (string)
  | EOF

val deb :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast_conf.configuration
