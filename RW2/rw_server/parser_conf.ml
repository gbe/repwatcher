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

open Parsing;;
# 25 "parser_conf.mly"
  open Ast_conf
# 22 "parser_conf.ml"
let yytransl_const = [|
  257 (* DQUOTE *);
  258 (* EQUAL *);
  259 (* PVIRGULE *);
  260 (* DIRECTORIES *);
  261 (* MODE *);
  262 (* SPEC *);
  263 (* UNWANTED *);
  264 (* SQL_LOGIN *);
  265 (* SQL_PSWD *);
  266 (* SQL_HOST *);
  267 (* SQL_PORT *);
  268 (* SQL_DBNAME *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  269 (* TXT *);
  270 (* DIGITS *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\006\000\008\000\007\000\007\000\005\000\
\005\000\003\000\003\000\004\000\004\000\009\000\009\000\000\000"

let yylen = "\002\000\
\028\000\001\000\003\000\003\000\003\000\002\000\003\000\001\000\
\001\000\003\000\003\000\002\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\004\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\000\011\000\000\000\000\000\000\000\
\000\000\000\000\000\000\014\000\008\000\009\000\000\000\012\000\
\000\000\000\000\000\000\005\000\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\007\000\000\000\000\000\
\001\000"

let yydgoto = "\002\000\
\004\000\007\000\016\000\027\000\028\000\029\000\050\000\030\000\
\031\000"

let yysindex = "\003\000\
\009\255\000\000\014\255\000\000\008\255\013\255\007\255\000\000\
\017\255\008\255\018\255\000\000\000\000\026\255\016\255\022\255\
\028\255\029\255\030\255\000\000\000\000\008\255\004\255\031\255\
\033\255\001\255\023\255\000\000\000\000\000\000\032\255\000\000\
\035\255\036\255\038\255\000\000\038\255\011\255\000\000\034\255\
\039\255\038\255\027\255\040\255\008\255\037\255\042\255\044\255\
\005\255\041\255\000\000\045\255\047\255\000\000\038\255\047\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\043\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\028\000\000\000\000\000\222\255\251\255\000\000\000\000\
\000\000"

let yytablesize = 53
let yytable = "\008\000\
\039\000\032\000\040\000\001\000\013\000\051\000\010\000\043\000\
\006\000\010\000\024\000\011\000\003\000\009\000\033\000\005\000\
\008\000\012\000\052\000\014\000\056\000\017\000\018\000\009\000\
\033\000\009\000\015\000\019\000\020\000\021\000\034\000\022\000\
\025\000\026\000\035\000\036\000\044\000\037\000\038\000\046\000\
\042\000\045\000\041\000\048\000\049\000\054\000\057\000\047\000\
\055\000\023\000\013\000\000\000\053\000"

let yycheck = "\005\000\
\035\000\001\001\037\000\001\000\010\000\001\001\003\001\042\000\
\001\001\003\001\007\001\005\001\004\001\013\001\014\001\002\001\
\022\000\001\001\014\001\002\001\055\000\006\001\007\001\013\001\
\014\001\013\001\001\001\006\001\001\001\001\001\008\001\002\001\
\002\001\001\001\003\001\001\001\010\001\002\001\001\001\045\000\
\002\001\002\001\009\001\002\001\001\001\001\001\000\000\011\001\
\002\001\022\000\008\001\255\255\012\001"

let yynames_const = "\
  DQUOTE\000\
  EQUAL\000\
  PVIRGULE\000\
  DIRECTORIES\000\
  MODE\000\
  SPEC\000\
  UNWANTED\000\
  SQL_LOGIN\000\
  SQL_PSWD\000\
  SQL_HOST\000\
  SQL_PORT\000\
  SQL_DBNAME\000\
  EOF\000\
  "

let yynames_block = "\
  TXT\000\
  DIGITS\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 25 : 'txt_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 22 : 'mode) in
    let _9 = (Parsing.peek_val __caml_parser_env 19 : 'txt_list) in
    let _12 = (Parsing.peek_val __caml_parser_env 16 : 'txt_digits_list_star) in
    let _15 = (Parsing.peek_val __caml_parser_env 13 : 'txt_or_digits) in
    let _18 = (Parsing.peek_val __caml_parser_env 10 : 'txt_or_digits) in
    let _21 = (Parsing.peek_val __caml_parser_env 7 : 'txt) in
    let _24 = (Parsing.peek_val __caml_parser_env 4 : 'digits_int_option) in
    let _27 = (Parsing.peek_val __caml_parser_env 1 : 'txt_or_digits) in
    Obj.repr(
# 64 "parser_conf.mly"
     ( 
      {
	c_directories = _3;
	c_mode = _6;
	c_specified_programs = _9;
	c_unwanted_programs = _12;
	c_sql = {
	  dbhost = Some _21;
	  dbname = Some _27;
	  dbport = _24;
	  dbpwd  = Some _18;
	  dbuser = Some _15;
	};
      }
    )
# 159 "parser_conf.ml"
               : Ast_conf.configuration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'txt) in
    Obj.repr(
# 83 "parser_conf.mly"
      ( [_1] )
# 166 "parser_conf.ml"
               : 'txt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'txt_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'txt) in
    Obj.repr(
# 84 "parser_conf.mly"
                        ( _3::_1 )
# 174 "parser_conf.ml"
               : 'txt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 88 "parser_conf.mly"
                    ( _2 )
# 181 "parser_conf.ml"
               : 'txt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 92 "parser_conf.mly"
                       ( _2 )
# 188 "parser_conf.ml"
               : 'digits))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser_conf.mly"
                ( None )
# 194 "parser_conf.ml"
               : 'digits_int_option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 97 "parser_conf.mly"
                       ( Some (int_of_string _2) )
# 201 "parser_conf.ml"
               : 'digits_int_option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'txt) in
    Obj.repr(
# 101 "parser_conf.mly"
      ( _1 )
# 208 "parser_conf.ml"
               : 'txt_or_digits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'digits) in
    Obj.repr(
# 102 "parser_conf.mly"
         ( _1 )
# 215 "parser_conf.ml"
               : 'txt_or_digits))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser_conf.mly"
                     ( Specified_programs )
# 221 "parser_conf.ml"
               : 'mode))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser_conf.mly"
                         ( Unwanted_programs )
# 227 "parser_conf.ml"
               : 'mode))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser_conf.mly"
                ( [] )
# 233 "parser_conf.ml"
               : 'txt_digits_list_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'txt_digits_list) in
    Obj.repr(
# 112 "parser_conf.mly"
                  ( _1 )
# 240 "parser_conf.ml"
               : 'txt_digits_list_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'txt_or_digits) in
    Obj.repr(
# 116 "parser_conf.mly"
                ( [_1] )
# 247 "parser_conf.ml"
               : 'txt_digits_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'txt_digits_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'txt_or_digits) in
    Obj.repr(
# 117 "parser_conf.mly"
                                         ( _3::_1 )
# 255 "parser_conf.ml"
               : 'txt_digits_list))
(* Entry deb *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let deb (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast_conf.configuration)
