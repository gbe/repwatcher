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


open Unix
open Types
open Types_conf


let log (txt, log_level) =

  let conf = Config.get() in
  let to_log = Printf.sprintf "%s\t%s\n" (Date.date()) txt in


  let rec open_fd ?(l=[ O_WRONLY ; O_APPEND]) log_filename = 
    try
      let fd = Unix.openfile log_filename l 0o666 in
      
      (* if true then it means the exception was triggered before *)
      if List.mem O_CREAT l then begin
	Unix.fchmod fd 0o666
      end;
	
	Some fd
	
      
    with Unix_error (err,_,_) ->
      
      match err with
      | ENOENT -> (* no such file or directory *)
	  open_fd ~l:[ O_WRONLY ; O_APPEND ; O_CREAT ] log_filename
      | _      ->
	  
	  (* Disable logging *)
	  Config.conf := Some {conf with c_log = Disabled};
	  
	  let error = Printf.sprintf "Oops. Couldn't log due to this Unix error: \
	      %s. Logging feature disabled" (Unix.error_message err) in
	    prerr_endline error;
	    
	    (* fd = None because it failed to open *)
	    None
  in


  let log_it ()  = 
    
    let log_filename =
      match log_level with
	  | (Normal  | Normal_Extra) -> "rw.log"
	  | Error                    -> "rw.log.err"
    in

      match open_fd log_filename with
	| None -> prerr_endline "An error occured, the file to log could neither \
	      be opened nor created"
	| Some fd ->
	    try
	      ignore (Unix.write fd to_log 0 (String.length to_log));
	      Unix.close fd
		
	    with _ ->
	      prerr_endline "An error occured either trying to log in the file \
		or to close it"
  in

    begin
      (* Only the Errors get printed and on stderr *)
      match log_level with
	| Normal -> ()
	| Normal_Extra -> ()
	| Error                    ->
	    prerr_endline to_log ;
	    Pervasives.flush Pervasives.stdout
    end;


    (* Depending on the user choice, we log *)
    match conf.c_log with
      | Disabled -> () (* don't log *)
      | Regular  ->
	  begin
	    match log_level with
	      | Normal       -> log_it ()
	      | Normal_Extra -> () (* don't log *)
	      | Error        -> log_it ()
	  end
      | Debug -> log_it ()
;;
