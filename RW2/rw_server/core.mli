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

module Core :
sig
	val fd : Unix.file_descr

	val add_watch : string -> Inotify.wd option -> bool -> unit
	val add_watch_children : string list -> unit
	val ls_children : string -> string list
	val print_ht : unit -> unit
	val what_to_do : Inotify.event -> unit
end

