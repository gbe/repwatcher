(*
    Repwatcher
    Copyright (C) 2009-2011  Gregory Bellier

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
  | comment                            { newline lexbuf; nexttoken lexbuf }
  | '='                                { EQUAL }
  | ';'                                { PVIRGULE }
  | "client_ca_path"                   { CLIENT_CA_PATH }
  | "client_cert_path"                 { CLIENT_CERT_PATH }
  | "client_key_path"                  { CLIENT_KEY_PATH }
  | "client_key_pwd"                   { CLIENT_KEY_PWD }
  | eof                                { EOF }
  | '\n'                               { newline lexbuf; nexttoken lexbuf }
  | space+                             { nexttoken lexbuf }
  | txt                                { let s = lexeme lexbuf in TXT (String.sub s 1 ((String.length s)-2) )}
  | _                                  { raise (Lexing_error (lexeme lexbuf)) }
