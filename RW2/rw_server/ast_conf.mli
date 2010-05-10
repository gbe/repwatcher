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


type sql_db = Mysql.db = {
  dbhost : string option;
  dbname: string option;
  dbport : int option;
  dbpwd : string option;
  dbuser : string option
}

type mode = Specified_programs | Unwanted_programs
    
type configuration={
  c_directories : string list;
  c_ignore_directories : string list;
  c_mode : mode;
  c_specified_programs : string list;
  c_unwanted_programs : string list;
  c_sql : sql_db;
  c_notify_loc : bool;
  c_notify_rem : bool;
}
