(*
    This file is part of Repwatcher.

    Repwatcher is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Repwatcher is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Repwatcher; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*)



{
  open Lexing
  open Parser
   
  exception Lexing_error of string
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}


let alpha = ['a'-'z' 'A'-'Z'] | ['\xE0'-'\xFF']

(*
specialchars =
  ! guillemet #$%&'()*+,-.
  :;<=>?@
  [\]^_`{|}~
*)

let specialchars = ['!'-'.' ':'-'@' '['-'`' '{'-'~'] (* Tout sauf le / *)

let digit = ['0'-'9']

let space = [' ' '\t']

let comment = '#'(alpha | digit | ['/'] | space | specialchars)*

let txt = (alpha | digit | ['/'] | ['_'] | ['.'] | ['-'] | ['+'])+


rule nexttoken = parse
  | comment               { nexttoken lexbuf }
  | '\"'                  { DQUOTE }
  | '='                   { EQUAL }
  | ';'                   { PVIRGULE }
  | "directories"         { DIRECTORIES }
  | "ignore_directories"  { IGNORE_DIRECTORIES }
  | "ignore_users"        { IGNORE_USERS }
  | "mode"                { MODE }
  | "specified_programs"  { SPEC }
  | "unwanted_programs"   { UNWANTED }
  | "sql_login"           { SQL_LOGIN }
  | "sql_pswd"            { SQL_PSWD }
  | "sql_host"            { SQL_HOST } 
  | "sql_port"            { SQL_PORT }
  | "sql_dbname"          { SQL_DBNAME }
  | "notify_locally"      { NOTIFY_LOCALLY }
  | "notify_remotely"     { NOTIFY_REMOTELY }
  | "log_level"           { LOG_LEVEL }
  | "Y"                   { YES }
  | "N"                   { NO }
  | eof                   { EOF }
  | '\n'                  { newline lexbuf; nexttoken lexbuf }
  | space+                { nexttoken lexbuf }
  | digit+               { DIGITS(lexeme lexbuf) }
  | txt                   { TXT(lexeme lexbuf) }
  | _     { raise (Lexing_error (lexeme lexbuf)) }
