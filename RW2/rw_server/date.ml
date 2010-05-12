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

open Unix

let date () =
  let date = (localtime(time())) in
    (* Starts in 1900 *)
    (* month = 0..11 *)
    
    (* 2008-01-19 16:21:00 *)
    Printf.sprintf "%02d-%02d-%02d %02d:%02d:%02d" (1900 + date.tm_year) (1 + date.tm_mon)     date.tm_mday date.tm_hour date.tm_min date.tm_sec
;;
