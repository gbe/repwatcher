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
open Ast
open Report


let certfile = ref "cert/rw_serv.crt"
let privkey  = ref "cert/rw_serv.key"
let password = ref "soso"

let ca = "CA/CA.crt"

let port         = ref 9292
let backlog      = 15

let reg          = Str.regexp "\n"

(* Return the IP from the socket *)
let get_ip sockaddr_cli =
  let inet_addr_of_sockaddr = function
    | Unix.ADDR_INET (n, _) -> n
    | Unix.ADDR_UNIX _ -> Unix.inet_addr_any
  in
  let inet_addr = inet_addr_of_sockaddr sockaddr_cli  in
    Unix.string_of_inet_addr inet_addr
;;

let get_common_name cert =
  let pat = Str.regexp "/" in	
  let cn = Str.regexp "CN=" in

  let rec loop = function
    | [] -> "Unknown user"
    | h :: q ->
	if Str.string_match cn h 0 then
	  let lastpos = Str.match_end() in
	    String.sub h lastpos ((String.length h)-lastpos)
	else
	  loop q
  in
    loop (Str.split pat cert)
;;


let run tor tow2 =

  (* Drop privileges by changing the processus' identity if its current id is root *)
  if Unix.geteuid() = 0 && Unix.getegid() = 0 then
    begin
      try
	(* Check in the file /etc/passwd if the user "nobody" exists *)
	let passwd_entry = Unix.getpwnam "nobody" in
	  setgid passwd_entry.pw_gid;
	  setuid passwd_entry.pw_uid;	
      with Not_found -> failwith "Fatal error. User 'nobody' doesn't exist. The network process can't take this identity"
    end;

  let connected_clients = ref [] in
  let m = Mutex.create () in

  Ssl_threads.init ();
  Ssl.init ();

  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.bind sock (ADDR_INET (inet_addr_any, !port));
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.listen sock backlog;


  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Server_context in

    if !password <> "" then
      Ssl.set_password_callback ctx (fun _ -> !password);

  begin
    try
      Ssl.use_certificate ctx !certfile !privkey;
    with Ssl.Private_key_error ->
      let error = "Err. Ssl_server: wrong private key password" in
      Report.report (Log (error, Level_1)) ;
      failwith error
  end;
  
    Ssl.set_verify ctx [Ssl.Verify_peer] (Some Ssl.client_verify_callback);

    (try
       Ssl.load_verify_locations ctx ca (Filename.dirname ca)
     with Invalid_argument e -> failwith ("Error_load_verify_locations: "^e))
    ;


    let send ser_txt socks =
      
      let do_it (ssl_s, sockaddr_cli, common_name) =

	try 
	  Ssl.output_string ssl_s ser_txt;
(*	  let log_msg = ref (Printf.sprintf "Sent '%s' to %s (%s)" ser_txt common_name (get_ip sockaddr_cli)) in
	    log_msg := Str.global_replace reg " " !log_msg;
	    Report.report (Log (!log_msg, Level_1))
*)
	      
	with Ssl.Write_error _ ->
	  Report.report (Log ("SSL write error\n", Level_1))
      in

	match socks with
	  | None ->
	      Mutex.lock m ;
	      List.iter do_it !connected_clients;
	      Mutex.unlock m
	  | Some sock' -> do_it sock'
    in

      
    let handle_interrupt i = 

      let ser_info = Marshal.to_string (Info_notif "rw_server_exit") [Marshal.No_sharing] in      
      send ser_info None;
      
      Mutex.lock m ;
      (* Close the clients' sockets *)
      List.iter (fun (ssl_s,_,_) -> Ssl.flush ssl_s ; Ssl.shutdown ssl_s) !connected_clients;
      Mutex.unlock m ;
      Unix.shutdown sock SHUTDOWN_ALL;
      exit 0
    in
      
     
      
    let pipe_waits_for_notifications () =
      let bufsize = 2048 in
      let buf = String.create bufsize in
	
      while true do
	let recv = Unix.read tor buf 0 bufsize in
	if recv > 0 then
	  begin
	    let data = String.sub buf 0 recv in
	    let notif = (Marshal.from_string data 0 : Ast.notification) in
            match notif with
               | (New_notif _ | Info_notif _ ) -> send data None
               | Old_notif _ -> send data (Some (List.hd !connected_clients))
	  end
      done
    in
      

      
    let client_quit sock_cli =
      
      Mutex.lock m ;
      let (l_clients_left, l_client) = List.partition ( fun (s,_,_) ->
							  s != sock_cli
						      ) !connected_clients
      in
	connected_clients := l_clients_left ;
	Mutex.unlock m ;

	let (_,sockaddr_cli,common_name) = List.hd l_client in
	let log_msg = Printf.sprintf "%s has quit (%s)" common_name (get_ip sockaddr_cli) in
	  Report.report ( Log (log_msg, Level_1) );
	  Ssl.shutdown sock_cli
    in
      

    let handle_connection (ssl_s, sockaddr_cli) =
      
      let cert = Ssl.get_certificate ssl_s in
      let subj = Ssl.get_subject cert in	
      let common_name = get_common_name subj in

      let new_client = Printf.sprintf "%s connects from %s" common_name (get_ip sockaddr_cli) in
      print_endline new_client;
      Report.report ( Log (new_client, Level_1) );
      
      Mutex.lock m ;
      connected_clients := (ssl_s, sockaddr_cli, common_name) :: !connected_clients;
      Mutex.unlock m ;

      let ser_info = Marshal.to_string (Info_notif "rw_server_con_ok") [Marshal.No_sharing] in
      send ser_info (Some(ssl_s, sockaddr_cli, common_name));

      let msg_new_client = "ask_current_dls" in
      ignore (Unix.write tow2 msg_new_client 0 (String.length msg_new_client));

      let loop = ref true in	
	
	try  
	  (* Wait for client exit *)	
	  while !loop do
	    let msg = Ssl.input_string ssl_s in
	      
	      if msg = "rw_client_exit" then
		begin
	          client_quit ssl_s;
		  loop := false
		end 
	  done
	with Ssl.Read_error _ -> client_quit ssl_s
    in
      
      
    ignore (Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_interrupt));
    ignore (Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt));
    
      
    ignore (Thread.create pipe_waits_for_notifications ());
      
    Printf.printf "Waiting for connections...\n";
    Pervasives.flush Pervasives.stdout;
      
    while true do
      
      let (s_cli, sockaddr_cli) = Unix.accept sock in
      let ssl_s = Ssl.embed_socket s_cli ctx in
      
      try
	Ssl.accept ssl_s;
	ignore ( Thread.create handle_connection (ssl_s, sockaddr_cli) )
      with
      | Invalid_argument _ ->
	  prerr_endline "Erreur dans le thread cote serveur!! ;o)" ;
	  Pervasives.flush Pervasives.stdout
      | Ssl.Accept_error _ ->
	  prerr_endline "A connection failed"
    done
	
	

