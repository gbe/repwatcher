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


let port         = ref 9292
let backlog      = 15

let m = Mutex.create () 
let connected_clients = ref []

(*
 * Probably because of marshalling, com type is 'a.
 * This caused some problems discovered at run time.
 * It's better to fix the type so the compiler can check if everything is ok.
 *) 
let tellserver (com : com_net2main) =
  let str_com = Marshal.to_string com [Marshal.No_sharing] in
  ignore (Unix.write Pipe.tow2 str_com 0 (String.length str_com))
;;


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



(* Send the notification to one or several clients *)
let send (com : com_server2clients) sock_opt =
  
  let ser_com = Marshal.to_string com [Marshal.No_sharing] in

  let do_it (ssl_s, sockaddr_cli, common_name) =
    try 
      Ssl.output_string ssl_s ser_com
    with Ssl.Write_error _ ->
      tellserver (Types.Log ("SSL write error", Error))
  in
  
  (* If sock is not given then it means the
   * notification must be sent to every one *)
  match sock_opt with
  | None ->
      Mutex.lock m ;
      List.iter do_it !connected_clients;
      Mutex.unlock m
  | Some sock' -> do_it sock'
;;


(* Listen for new notifications *)
let pipe_waits_for_notifications tor =
  let bufsize = 2048 in
  let buf = String.create bufsize in
  
  while true do
    let recv = Unix.read tor buf 0 bufsize in
    if recv > 0 then
      begin
	let data = String.sub buf 0 recv in
	let notif = (Marshal.from_string data 0 : Types.notification) in
	(* Resend the data already serialized to the clients *)
        match notif with
	| Local_notif _ -> assert false
        | New_notif _   -> send (Notification notif) None
        | Old_notif _   -> send (Notification notif) (Some (List.hd !connected_clients))
      end
  done
;;



(* Remove the client from the connected clients list *)
let client_quit sock_cli =
  
  Mutex.lock m ;
  let (l_clients_left, l_client) =
    List.partition (
    fun (s,_,_) -> s != sock_cli
   ) !connected_clients
  in
  connected_clients := l_clients_left ;
  Mutex.unlock m ;
  
  let (_,sockaddr_cli,common_name) = List.hd l_client in
  let log_msg = Printf.sprintf "%s has quit (%s)" common_name (get_ip sockaddr_cli) in
  print_endline log_msg;
  tellserver ( Types.Log (log_msg, Normal) );
  Ssl.shutdown sock_cli
;;




(* This is where a new connection is processed *)
(* This function runs in a dedicated thread *)
let handle_connection (ssl_s, sockaddr_cli) =
  
  let cert = Ssl.get_certificate ssl_s in
  let subj = Ssl.get_subject cert in	
  let common_name = get_common_name subj in
  
  let new_client = Printf.sprintf "%s connected from %s" common_name (get_ip sockaddr_cli) in
  print_endline new_client;
  tellserver (Types.Log (new_client, Normal)) ;
  
  Mutex.lock m ;
  connected_clients := (ssl_s, sockaddr_cli, common_name) :: !connected_clients;
  Mutex.unlock m ;

(* To be moved from there *)
  let conf = Config.get() in
  
  (* Tell the new client that he is authorized and send him at the same time the number of last folders to display set in the config file*)
  send (RW_server_con_ok conf.c_notify.n_parent_folders) (Some(ssl_s, sockaddr_cli, common_name));
  
  (* Ask father's process for the current downloads to send them to the new client *)
  tellserver Ask_current_dls;
  
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
;;




let run tor remote_config =


  
(* Initialize SSL in this area while the chroot
   has not been done yet so we can access the certificate and
   private key *)
  Ssl_threads.init ();
  Ssl.init ();

  let certs =
    match remote_config.r_cert with
      | None -> assert false
      | Some certs -> certs
  in

  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Server_context in

  if certs.c_serv_key_pwd <> "" then
      Ssl.set_password_callback ctx (fun _ -> certs.c_serv_key_pwd);

  begin
    try
      Ssl.use_certificate ctx certs.c_serv_cert_path certs.c_serv_key_path;
    with
      | Ssl.Private_key_error ->
	let error = "Err. Ssl_server: wrong private key password" in
	tellserver (Types.Log (error, Error)) ;
	failwith error
      | Ssl.Certificate_error ->
	tellserver (Types.Log ("Certificate error", Error));
	raise Ssl.Certificate_error
  end;
  
  Ssl.set_verify ctx [Ssl.Verify_peer] (Some Ssl.client_verify_callback);
  
  begin
    try
      Ssl.load_verify_locations ctx certs.c_ca_path (Filename.dirname certs.c_ca_path)
    with Invalid_argument e ->
      let error = ("Error_load_verify_locations: "^e) in
      tellserver ( Types.Log (error, Error)) ;
      failwith error
  end;
(* ********************** *)





  (* Operations on the process itself *)
  
  (* If the processus' identity is root *)
  if Unix.geteuid() = 0 && Unix.getegid() = 0 then
    begin
      
      (* Check needs to be done before chrooting *)
      let check_identity identity =
	match identity with
	| None -> None
	| Some new_remote_id ->
	    try
	      (* Check in the file /etc/passwd if the user new_remote_id exists *)
	      Some (Unix.getpwnam new_remote_id)
	    with Not_found ->
	      let error = ("Fatal error. User '"^new_remote_id^"' doesn't exist. The network process can't take this identity") in
	      tellserver ( Types.Log (error, Error));
	      failwith error
      in
      let passwd_entry_opt = check_identity remote_config.r_process_identity in
      
      (* Chroot the process *)
      begin
	try
	  match remote_config.r_chroot with
	  | None -> ()
	  | Some dir ->
	      Unix.chdir dir ; Unix.chroot "." ;
	      tellserver ( Types.Log (("Network process chrooted in "^dir), Normal_Extra))
	with Unix_error (error,_,s2) ->
	  let error = ("Remote process can't chroot in '"^s2^"': "^(Unix.error_message error)) in
	  tellserver (Types.Log (error, Error));
	  failwith error
      end;

      (* Change the effective uid and gid after the chroot *)
      begin
	match passwd_entry_opt with
	| None -> ()
	| Some passwd_entry -> 
	    setgid passwd_entry.pw_gid;
	    setuid passwd_entry.pw_uid;
	    
	    match remote_config.r_process_identity with
	    | None -> assert false
	    | Some new_remote_id -> tellserver (Types.Log (("Network process identity changed to "^new_remote_id), Normal_Extra))
      end;
    end;

  (* *********************** *)





  (* Initialize Unix socket *)
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.bind sock (ADDR_INET (inet_addr_any, !port));
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.listen sock backlog;
  (* ********************** *)




  let clear_exit () =
    send RW_server_exited None;
    
    Mutex.lock m ;
    (* Close the clients' sockets *)
    List.iter (fun (ssl_s,_,_) -> Ssl.flush ssl_s ; Ssl.shutdown ssl_s) !connected_clients;
    Mutex.unlock m ;
    Unix.shutdown sock SHUTDOWN_ALL
  in

  at_exit clear_exit;
  (* *********************** *)
  

    
  ignore (Thread.create pipe_waits_for_notifications tor);

  print_endline "Waiting for connections...";
  
  while true do
    
    let (s_cli, sockaddr_cli) = Unix.accept sock in
    let ssl_s = Ssl.embed_socket s_cli ctx in
    
    try
      Ssl.accept ssl_s;
      ignore ( Thread.create handle_connection (ssl_s, sockaddr_cli) )
    with
    | Invalid_argument _ ->
	let error = "Error in the thread, server-side" in
	tellserver (Types.Log (error, Error)) ;
	prerr_endline error
    | Ssl.Accept_error _ ->
	let error = "A connection failed" in
	tellserver (Types.Log (error, Error)) ;
	prerr_endline error
  done
;;
	
