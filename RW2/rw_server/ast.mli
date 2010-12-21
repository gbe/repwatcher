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


type f_file = {
  f_name        : string ;
  f_path        : string ;
  f_login       : string ;
  f_filesize    : int64  ;
  f_prog_source : string ;
}

type file_state = File_Opened | File_Closed

(* New log_level type *)
type log_level = | Normal
		 | Normal_Extra
		 | Error



(* New of file
 * Old of file * date) list
 * Info of message (such as Repwatcher is watching you)
*)

type notification =
  | New_notif  of f_file * file_state
  | Old_notif  of (f_file * string) list
  | Info_notif of string

type report = | Notify of notification
	      | Log    of (string * log_level)
	      | Sql    of (f_file * file_state)

type 'a query_result = 
  | QueryOK    of 'a
  | QueryEmpty
  | QueryError of string



