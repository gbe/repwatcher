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


open Ast
open Ast_conf
open Mysql
open Unix


(* connection identifier *)
let cid = ref None;;

let connect () =
  cid := Some (Mysql.connect (Config.get()).c_sql)
;;

let query q =

  connect();

  match !cid with
    | None -> assert false
    | Some cid ->

	let res = exec cid q in
	let status_query = status cid in
	  
	  disconnect cid;
	  
	  match status_query with
	    | StatusOK      -> QueryOK res
	    | StatusEmpty   -> QueryEmpty
	    | StatusError _ ->
		match errmsg cid with
		  | None         -> QueryError "Oops. Mysqldb.query, StatusError returned a None. This is not supposed to happen"
		  | Some errmsg' -> QueryError errmsg'
;;


let fetch q =

  match query q with
    | QueryOK res          -> QueryOK (Mysql.fetch res)
    | QueryEmpty           -> QueryEmpty
    | QueryError error_msg -> QueryError error_msg
;;
