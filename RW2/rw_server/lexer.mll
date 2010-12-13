(*
    Repwatcher
    Copyright (C) 2009-2010  Gregory Bellier

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

let space = [' ' '\t']

let comment = '#'([^'\n'])*'\n'

let txt = '\"' [^'\"']* '\"'


rule nexttoken = parse
  | comment               { newline lexbuf; nexttoken lexbuf }
  | '='                   { EQUAL }
  | ';'                   { PVIRGULE }
  | "directories"         { DIRECTORIES }
  | "ignore_directories"  { IGNORE_DIRECTORIES }
  | "ignore_users"        { IGNORE_USERS }
  | "specified_programs"  { SPECIFIED_PROGRAMS }
  | "unwanted_programs"   { UNWANTED_PROGRAMS }
  | "sql_login"           { SQL_LOGIN }
  | "sql_pswd"            { SQL_PSWD }
  | "sql_host"            { SQL_HOST }
  | "sql_port"            { SQL_PORT }
  | "sql_dbname"          { SQL_DBNAME }
  | "notify_locally"      { NOTIFY_LOCALLY }
  | "notify_remotely"     { NOTIFY_REMOTELY }
  | "log_level"           { LOG_LEVEL }  
  | eof                   { EOF }
  | '\n'                  { newline lexbuf; nexttoken lexbuf }
  | space+                { nexttoken lexbuf }
  | txt                   { let s = lexeme lexbuf in TXT (String.sub s 1 ((String.length s)-2) )}
  | _                     { raise (Lexing_error (lexeme lexbuf)) }
