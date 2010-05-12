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

let cid = ref None;;

let connect c_sql =
  cid := Some (Mysql.connect c_sql)
;;

let query q =
  print_string q;
  
  match !cid with
    | None -> failwith "No cid"
    | Some cid -> exec cid q
;;

let fetch q =
  Mysql.fetch (query q)
;;
