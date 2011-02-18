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
open Types_conf

let get channel =
  
  let l = ref [] in    
  let regexp_cut_space = Str.regexp "[' ']+" in
  let regexp_quote     = Str.regexp "'" in
  let regexp_quote2    = Str.regexp "&apos;" in

  let file = {f_name="";f_path="";f_login="";f_filesize=(Int64.of_int 0);f_prog_source=""} in
  
  begin
    try
      while true do 
	let line     = Str.global_replace regexp_quote "&apos;" (input_line channel) in
	let cut_line = Str.split regexp_cut_space line in

	let rec set_file cut_line column file =  
	  match cut_line with
	  | [] -> file
	  | t::q -> if column = 0 then
	      set_file q (column+1) {file with f_prog_source = t}
		
	  else if List.mem column [1 ; 3 ; 4 ; 5 ; 7] then
	    set_file q (column+1) file
	      
	      (* login *)
	  else if column = 2 then
	    set_file q (column+1) {file with f_login = t}
	      
	      (* file size *)
	  else if column = 6 then
	    set_file q (column+1) {file with f_filesize = (Int64.of_string t)}
	      
	  else
	    let path_and_filename = Str.global_replace regexp_quote2 "'" (t^" "^(String.concat " " q)) in
	    {file with f_name = (Filename.basename path_and_filename) ; f_path = (Filename.dirname path_and_filename)^"/"}
	      
	in l := (set_file cut_line 0 file)::(!l)
      done
    with _ -> close_in channel
  end;
  !l
;;




(* list unfiltered -> list filtered. The filter depends on the mode and if the user is ignored *)
let filter unfiltered_l =
  
  let conf = Config.get() in
    
  (* Remove all the files according to the specified or unwanted mode from the config file *)
  let lprogs_filtered =
    match conf.c_mode with
    | (Specified_programs, specified_programs) ->
	List.filter (fun file -> List.mem file.f_prog_source specified_programs) unfiltered_l
	    
    (* Remove all the unwanted_programs from the list *)
    | (Unwanted_programs, unwanted_programs) ->	  	  
	List.filter (fun file -> if List.mem file.f_prog_source unwanted_programs then false else true) unfiltered_l
	    
  in
    
    (* Remove all the files accessed by one of the ignored users *)
    List.filter (fun file ->
		   not (List.mem file.f_login conf.c_watch.w_ignore_users)
		) lprogs_filtered
;;	    
