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


(* New of login * filename
 * Old of login * filename * date) list
 * Info of message (such as Repwatcher is watching you)
*)

type file_state = File_Opened | File_Closed;;

type notification =
  | New_notif  of string * string * file_state
  | Old_notif  of (string * string * string) list
  | Info_notif of string
;;
