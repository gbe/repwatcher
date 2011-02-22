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


open Lexing
open Format

let conf = ref None;;

(* localize the error and give the line and column *)
let localization (pos,e) config_file =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  let lc = e.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, character %d-%d:\n" config_file l c lc
;;

let get () =
  match !conf with
    | None -> failwith "Failed to retrieve the configuration file"
    | Some c -> c
;;

let parse config_file =
  let c = open_in config_file in
  let lb = Lexing.from_channel c in
    try
      conf := Some (Parser.configuration Lexer.nexttoken lb) ;
      close_in c;
      get()
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
		  
