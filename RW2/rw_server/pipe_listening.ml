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


open Ast;;
open Report;;


let wait_pipe_from_child_process () =
  let bufsize = 1024 in
  let buf = String.create bufsize in
  
  while true do
    let recv = Unix.read Pipe.tor2 buf 0 bufsize in

    if recv > 0 then
      begin
	match String.sub buf 0 recv with
	| "ask_current_dls" ->

	    let l_current_dls = Hashtbl.fold (fun (_,file) date ret -> ret@[(file.f_login, (Txt_operations.escape_for_notify file.f_name), date)] ) Files_progress.ht [] in
	    
	    (* We go through the pipe only if it's necessary *)
	    if List.length l_current_dls > 0 then
	      begin
		let str_current_dls = Marshal.to_string (Old_notif l_current_dls) [Marshal.No_sharing] in
		ignore (Unix.write Pipe.tow str_current_dls 0 (String.length str_current_dls))
	      end
	| _ -> Report.report (Log ("Err. The server received an unknown command", Warning))
      end;
  done
;;
