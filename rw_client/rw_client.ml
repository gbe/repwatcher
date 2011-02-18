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
open Unix
(* open Dbus_call *)




let _ =

  let port = ref 9292 in
  let host = ref "" in
  let nb_parent_folders = ref (-1) in (* will be changed if set on the CLI or when connected to the server *)
  
  let password = ref "coco" in
  let certfile = ref "cert/rw_client.crt" in
  let privkey  = ref "cert/rw_client.key" in
  let ca = "CA/CA.crt" in
  
  let usage = "usage: rw_client host [-p port] [-n Folders_nb_for_notifications]" in
  
  Printf.printf "\nRepwatcher  Copyright (C) 2009-2011  Gregory Bellier
This program comes with ABSOLUTELY NO WARRANTY; for details read COPYING file.
This is free software, and you are welcome to redistribute it
under certain conditions; for details read COPYING file\n\n";
  Pervasives.flush Pervasives.stdout;

  let regexp = Str.regexp "/" in

  Arg.parse
    [
     "-p", Arg.Int (fun i -> port := i), "\tPort";
     "-n", Arg.Int (fun n ->
       if n < 0 then
	 raise (Arg.Bad ("wrong argument `"^(string_of_int n)^"'; option `-n' expects an unsigned integer"))
       else
	 nb_parent_folders := n
     ), "\tFolders_Number_For_Notifications";
    ]
    (fun s -> host := s) usage;
  
  if !host = "" then (Printf.printf "%s\n\n" usage; exit 1);
 
  Ssl_threads.init ();
  Ssl.init ();

  let he =
    (
     try
       gethostbyname !host
     with
     | Not_found -> failwith "Host not found"
    )
  in
  
  let sockaddr = ADDR_INET(he.h_addr_list.(0), !port) in
  let loop = ref true in
  let bufsize = 1024 in
  let buf = String.create bufsize in
  
  let ssl =
    let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
    
    if !password <> "" then
      Ssl.set_password_callback ctx (fun _ -> !password);
    
    Ssl.use_certificate ctx !certfile !privkey;      
    Ssl.set_verify ctx [Ssl.Verify_peer] (Some Ssl.client_verify_callback);
    begin try
      Ssl.load_verify_locations ctx ca (Filename.dirname ca)
    with Invalid_argument e -> failwith ("Error_load_verify_locations"^e)
    end;
    
    Ssl.set_verify_depth ctx 2;
    Ssl.open_connection_with_context ctx sockaddr
  in

      
  let handle_interrupt i =       
    begin try
      Ssl.output_string ssl "rw_client_exit"
    with Ssl.Write_error _ -> failwith "Failed to send the closing msg to server"
    end;
    Ssl.flush ssl;
    Ssl.shutdown_connection ssl;
    exit 0
  in
      
  ignore (Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_interrupt));
  ignore (Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt));

  let rec n_last_elements l =

    match l with
    | [] -> "./"
    | [t] -> t^"/"
    | t::q ->
	if List.length q < !nb_parent_folders then
	  t^"/"^(n_last_elements q)
	else
	  n_last_elements q
  in
  
  begin try
    while !loop do
      let data_recv = Ssl.read ssl buf 0 bufsize in
      
      if data_recv > 0 then
	let data = String.sub buf 0 data_recv in
	let com = (Marshal.from_string data 0 : com_server2clients) in
	
	match com with
	| RW_server_exited -> loop := false
	| RW_server_con_ok nb_parent_folders_from_server -> 
	    begin
	      match !nb_parent_folders with
		| -1 -> (* Otherwise, we take the server's choice *)
		  nb_parent_folders :=
		    begin match nb_parent_folders_from_server with
		      | None -> 0
		      | Some nb -> nb
		    end
		| _ -> () (* It has been set by the client on the command line. *)
	      
	    end;
	    ignore (Unix.system ("notify-send -i nobody Repwatcher \"Successfully connected to "^(!host)^"\""))
(*	    | RW_server_con_ok -> dbus "nobody" "Repwatcher" ("Successfully connected to "^(!host)) *)
	      
	| Notification notif ->
	    begin
	      match notif with
	      (* Local_notifs can only be done server-side *)
	      | Local_notif _ -> assert false

	      | New_notif (file, filestate) -> 
		  
		  let (str_of_state, msg_state) =
		    match filestate with
		    | File_Opened -> ("File_Opened", "is downloading")
		    | File_Closed -> ("File_Closed", "finished downloading")
		  in
		  
		  Printf.printf "Recu new_notif: '%s', '%s' et %s\n" file.f_login file.f_name str_of_state;
		  Pervasives.flush Pervasives.stdout;

		  let l_folders = Str.split regexp file.f_path in
		  let call =
		    match !nb_parent_folders with
		      | 0 -> Printf.sprintf "notify-send -i nobody Repwatcher \"<b>%s</b> %s\n%s\"" file.f_login msg_state file.f_name
		      | _ -> Printf.sprintf "notify-send -i nobody Repwatcher \"<b>%s</b> %s\n%s%s\"" file.f_login msg_state (n_last_elements l_folders) file.f_name
		  in
		  ignore (Unix.system call)
		    
(*		let dbus_notif = Printf.sprintf "<b>%s</b> %s\n%s" login msg_state filename in
  dbus "nobody" "Repwatcher" dbus_notif
 *)		  
		    
	      | Old_notif dls_l ->
		  let regexp_space = Str.regexp "[' ']" in
		  let regexp_dot = Str.regexp "[':']" in

		  List.iter (
		  fun (file, date) ->
		    Printf.printf "Recu Old_notif: '%s', '%s' et '%s'\n" file.f_login file.f_name date;
		    Pervasives.flush Pervasives.stdout;
		    
		    let h_m_s = List.hd (List.tl (Str.split regexp_space date)) in
		    let h_m = 
		      let hms_l = Str.split regexp_dot h_m_s in
		      (List.nth hms_l 0)^":"^(List.nth hms_l 1)
		    in

		    let l_folders = Str.split regexp file.f_path in

		    let call =
		      match !nb_parent_folders with
			| 0 -> Printf.sprintf "notify-send -i nobody \"Repwatcher @ %s\" \"<b>%s</b> started downloading\n%s\"" h_m file.f_login file.f_name
			| _ -> Printf.sprintf "notify-send -i nobody \"Repwatcher @ %s\" \"<b>%s</b> started downloading\n%s%s\"" h_m file.f_login (n_last_elements l_folders) file.f_name
		    in

		    ignore (Unix.system call)
		      
(*		  let dbus_notif = Printf.sprintf "<b>%s</b> started downloading\n%s" login filename in
 *		    dbus "nobody" ("Repwatcher @ "^h_m) dbus_notif
 *)
		 ) dls_l
	    end		
    done
  with Ssl.Read_error _ -> ()
  end;
  
  Ssl.shutdown ssl;
  ignore (Unix.system "notify-send -i nobody Repwatcher \"Server is down. Closing the client...\"");
(*      dbus "nobody" "Repwatcher" "Server is down. Closing the client..."; *)
  
  exit 0	   
;;
