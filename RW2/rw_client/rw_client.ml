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

open Notifications
open Unix

let _ =

  let port = ref 9292 in
  let host = ref "" in

  let password = ref "coco" in
  let certfile = ref "cert/rw_client.crt" in
  let privkey  = ref "cert/rw_client.key" in
  let ca = "CA/CA.crt" in

  let usage = "usage: rw_client host [-p port]" in

  Printf.printf "\nRepwatcher  Copyright (C) 2009  Gregory Bellier
This program comes with ABSOLUTELY NO WARRANTY; for details read COPYING file.
This is free software, and you are welcome to redistribute it
under certain conditions; for details read COPYING file\n\n";
  Pervasives.flush Pervasives.stdout;

  Arg.parse
    [
      "-p", Arg.Int (fun i -> port := i), "\tPort";
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
	(try
	   Ssl.load_verify_locations ctx ca (Filename.dirname ca)
	 with Invalid_argument e -> failwith ("Error_load_verify_locations"^e))
	;
	
	Ssl.set_verify_depth ctx 2;
	Ssl.open_connection_with_context ctx sockaddr
    in

      
    let handle_interrupt i =       
      (try
	 Ssl.output_string ssl "rw_client_exit"
       with Ssl.Write_error _ -> failwith "Failed to send the closing msg to server"
      );
      Ssl.flush ssl;
      Ssl.shutdown_connection ssl;
      exit 0
    in
      
    ignore (Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_interrupt));
    ignore (Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt));
  

    let regexp_space = Str.regexp "[' ']" in
    let regexp_dot = Str.regexp "[':']" in

      
      (try
	while !loop do
	  let data_recv = Ssl.read ssl buf 0 bufsize in
	  
	  if data_recv > 0 then
	    let data = String.sub buf 0 data_recv in
	    let notif = (Marshal.from_string data 0 : notification) in
	    
	    match notif with
	    | Info_notif info ->
		begin
		  match info with
		  | "rw_server_exit"   -> loop := false
		  | "rw_server_con_ok" -> ignore (Unix.system ("notify-send -i nobody Repwatcher \"Successfully connected to "^(!host)^"\""));
		  | _                  ->
		      assert false
		end
		  
	    | New_notif (login, filename, filestate) -> 

		let (str_of_state, msg_state) =
		  match filestate with
		  | File_Opened -> ("File_Opened", "is downloading")
		  | File_Closed -> ("File_Closed", "finished downloading")
		in

		Printf.printf "Recu new_notif: '%s', '%s' et %s\n" login filename str_of_state;
		Pervasives.flush Pervasives.stdout;
		let call = Printf.sprintf "notify-send -i nobody Repwatcher \"<b>%s</b> %s\n%s\"" login msg_state filename in
		ignore (Unix.system call)
		  
	    | Old_notif dls_l ->
		List.iter (
		fun (login, filename, date) ->
		  Printf.printf "Recu Old_notif: '%s', '%s' et '%s'\n" login filename date;
		  Pervasives.flush Pervasives.stdout;

		  let h_m_s = List.hd (List.tl (Str.split regexp_space date)) in
		  let h_m = 
		    let hms_l = Str.split regexp_dot h_m_s in
		    (List.nth hms_l 0)^":"^(List.nth hms_l 1)
		  in
		  let call = Printf.sprintf "notify-send -i nobody \"Repwatcher @ %s\" \"<b>%s</b> started downloading\n%s\"" h_m login filename in
		  ignore (Unix.system call)
	       ) dls_l
	done;
      with Ssl.Read_error _ -> ()
      );
      
      Ssl.shutdown ssl;
      ignore (Unix.system "notify-send -i nobody Repwatcher \"Server is down. Closing the client...\"");
      
      exit 0	   
