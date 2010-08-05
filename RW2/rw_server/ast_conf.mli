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

type sql_db = Mysql.db = {
  dbhost : string option ;
  dbname : string option ;
  dbport : int option    ;
  dbpwd  : string option ;
  dbuser : string option ;
}

type mode = Specified_programs | Unwanted_programs

type log_verbosity  = | Disabled
		      | Regular
		      | Debug

type configuration={
  c_directories        : string list   ;
  c_ignore_directories : string list   ; 
  c_ignore_users       : string list   ;
  c_mode               : mode          ;
  c_specified_programs : string list   ;
  c_unwanted_programs  : string list   ;
  c_sql                : sql_db        ;
  c_notify_loc         : bool          ;
  c_notify_rem         : bool          ;
  c_log_level          : log_verbosity ;
}
