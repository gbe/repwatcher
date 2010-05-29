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


type f_file = {
  f_name        : string ;
  f_path        : string ;
  f_login       : string ;
  f_filesize    : int64  ;
  f_prog_source : string ;
}

type files      = f_file list
type file_state = File_Opened | File_Closed
type log_level  = Level_1     | Level_2

type report = | Notify of string
	      | Log    of (string * log_level)
	      | Sql    of (f_file * file_state)

type 'a query_result = 
  | QueryOK    of 'a
  | QueryEmpty
  | QueryError of string
