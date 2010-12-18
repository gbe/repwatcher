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

type watch = {
    w_directories        : string list   ;
    w_ignore_directories : string list   ; 
    w_ignore_users       : string list   ;
}

type mode_t =  Specified_programs | Unwanted_programs
type mode = mode_t * string list


type sql_db = Mysql.db = {
    dbhost : string option ;
    dbname : string option ;
    dbport : int option    ;
    dbpwd  : string option ;
    dbuser : string option ;
}

type remote = {
    r_activate : bool;
    r_process_identity : string option;
  }
      
type notify = {
    n_locally  : bool;
    n_remotely : remote;
}

type log_verbosity  =
  | Disabled
  | Regular
  | Debug

type configuration = {
    c_watch : watch;
    c_mode : mode;
    c_main_proc_id_fallback : string option;
    c_mysql : sql_db;
    c_notify : notify;
    c_log : log_verbosity;
}
