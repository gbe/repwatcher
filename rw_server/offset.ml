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

open Types
open Fdinfo

let loop_check () =
  
  while true do

    Hashtbl.iter (fun (wd, file) (date, _) ->
      let pid = pid_of_int file.f_program_pid in

      let fds =
	try
	  get_fds pid
	with Unix.Unix_error _ -> []
      in
      
      let offset =
	try
	  let fd =
	    List.find (fun fd ->
	    (*  print_endline ("'"^fd.name^"'\net\n'"^file.f_path^file.f_name^"'\n"); *)
	      fd.name = (file.f_path^file.f_name)
		      ) (List.rev fds)
	  in
	  get_offset pid fd
	with
	| Unix.Unix_error _ -> Int64.of_int (-1)
	| Not_found -> Int64.of_int (-1)
      in

(*
      Printf.printf "Offset: %s\n" (Int64.to_string offset);
      Pervasives.flush Pervasives.stdout;
*)
      Hashtbl.replace Files_progress.ht (wd, file) (date, offset)

		 ) Files_progress.ht ;

    Unix.sleep 5
  done
;;
