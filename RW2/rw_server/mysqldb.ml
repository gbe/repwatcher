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

open Mysql
open Types
open Types_conf
open Unix

let ml2str = Mysql.ml2str;;

let connect () =
  try
    Some (Mysql.connect (Config.get()).c_mysql)
  with Mysql.Error error ->
    Log.log (error, Error);
    None
;;

let disconnect cid =
  try
    Mysql.disconnect cid;
    Log.log ("Disconnected from MySQL", Normal_Extra);
    true
  with Mysql.Error error ->
    Log.log ("RW couldn't disconnect from Mysql: "^error, Error);
    false
;;



let map res =
  try
    (* row = string option array, the option is if a field is NULL *)
    let rows = Mysql.map res (fun row -> row) in
    Log.log ("Query successfully mapped", Normal_Extra);

    rows

  with Mysql.Error error ->
    Log.log (error, Error);
    []
;;


let query q =

  (* Establish a connection with MySQL *)
  match connect () with
  | None -> [] (* could not connect so we do nothing *)
  | Some cid ->

      Log.log ("Connected to MySQL", Normal_Extra);
      Log.log (("Next SQL query to compute:\n"^q^"\n"), Normal_Extra);

      let rows_list =
	try
	  let res = exec cid q in
	  
	  match status cid with
	  | StatusOK      ->
	      Log.log ("Query successfully executed", Normal_Extra);
	      map res
	  | StatusEmpty   -> []
	  | StatusError _ ->
	      begin match errmsg cid with
	      | None ->
		  Log.log ("Oops. Mysqldb.query had an error and the SGBD \
			     can't tell which one", Error)
	      | Some errmsg' -> Log.log (errmsg', Error)
	      end;
	      []
		      with Mysql.Error error ->
			Log.log (error, Error);
			[]
      in
      ignore (disconnect cid);
      rows_list
;;



