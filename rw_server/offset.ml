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


let get pid filepath =
  let pid = pid_of_int pid in
  
  let fds =
    try
      get_fds pid
    with Unix.Unix_error (error, funct, arg) ->
      let err = Printf.sprintf "Offset. %s, in function %s, arg %s" (Unix.error_message error) funct arg in
      Log.log (err, Error) ;
      []
  in
  
  try
    let fd =
      List.find (fun fd -> fd.name = filepath) fds
    in
    get_offset pid fd
  with
    | Unix.Unix_error (error, funct, arg) ->
      let err = Printf.sprintf "\nOffset. %s, in function %s, arg %s" (Unix.error_message error) funct arg in
      Log.log ("ERROR A"^err, Error) ;
      Int64.of_int 0

    | Not_found ->
      Log.log ("ERROR B", Error) ;
      List.iter (fun fd -> Log.log (fd.name, Error)) fds ;
      Int64.of_int 0
;;




let loop_check () =
  
  while true do

    Hashtbl.iter (fun (wd, file) (date, _) ->
      let offset = get file.f_program_pid (file.f_path^file.f_name) in
      Hashtbl.replace Files_progress.ht (wd, file) (date, offset)
    ) Files_progress.ht ;
    
    Unix.sleep 5 ;
  done
;;