(*
    Repwatcher
    Copyright (C) 2009  Gregory Bellier

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


open Ast
open Ast_conf
open Mysql
open Unix


(* connection identifier *)
let cid = ref None;;

let ml2str = Mysql.ml2str;;

let connect () =
  cid := Some (Mysql.connect (Config.get()).c_sql)
;;

let disconnect () =
  match !cid with
  | None -> assert false
  | Some cid -> Mysql.disconnect cid
;;

let query q =

  connect();
  
  match !cid with
  | None -> assert false
  | Some cid ->

      try
	let res = exec cid q in

	let ret =
	  match status cid with
	  | StatusOK      -> QueryOK res
	  | StatusEmpty   -> QueryEmpty
	  | StatusError _ ->
	      match errmsg cid with
	      | None         -> QueryError "Oops. Mysqldb.query, StatusError returned a None. This is not supposed to happen"
	      | Some errmsg' -> QueryError errmsg'
	in
        disconnect ();
	ret

      with (Mysql.Error error) ->
	let ret =
	  match errmsg cid with
	  | None         -> QueryError ("Oops. "^error)
	  | Some errmsg' -> QueryError errmsg'
	in
	disconnect ();
	ret
;;


let fetch q =

  match query q with
    | QueryOK res          -> QueryOK (Mysql.fetch res)
    | QueryEmpty           -> QueryEmpty
    | QueryError error_msg -> QueryError error_msg
;;
