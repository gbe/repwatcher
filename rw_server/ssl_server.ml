open Unix
open Types
open Types_conf
open Printf



class ssl_server params tor =
  (*
   * Probably because of marshalling, com type is 'a.
   * This caused some problems discovered at run time.
   * It's better to fix the type so the compiler can check if everything is ok.
   *)
  let tellserver (com : com_net2main) =
    let str_com = Marshal.to_string com [Marshal.No_sharing] in
    ignore (Unix.write (Pipe.child2father#get_towrite) str_com 0 (String.length str_com))
  in

object(self)

  val default_port = 9292
  val backlog      = 15
  val sock = Unix.socket PF_INET SOCK_STREAM 0
  val clients_handle = new Ssl_handle_clients.ssl_handling_clients tellserver

  method initialize_ssl_ctx =
    (* Initialize SSL in this area while the chroot has not been done yet
     * so we can access the certificate and private key *)
    Ssl_threads.init ();
    Ssl.init ();

    let certs =
      match params.s_certs with
      | None -> assert false
      | Some certs -> certs
    in

    let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Server_context in

    begin
      match certs.c_serv_key_pwd with
      | None -> ()
      | Some pwd -> Ssl.set_password_callback ctx (fun _ -> pwd)
    end;

    begin
      try
	Ssl.use_certificate ctx certs.c_serv_cert_path certs.c_serv_key_path;
      with
      | Ssl.Private_key_error ->
	let error = "Err. Ssl_server: wrong private key password" in

	(* Because of this error, RW's run is aborted *)
	(* Logging is done in the other process *)
	tellserver (Exit_on_error error) ;
	exit 1

      | Ssl.Certificate_error ->
	(* Because of this error, RW's run is aborted *)
	(* Logging is done in the other process *)
	tellserver (Exit_on_error "Certificate error") ;
	exit 1
    end;

    Ssl.set_verify
      ctx
      [Ssl.Verify_fail_if_no_peer_cert]
      (Some Ssl.client_verify_callback);

    begin
      try
	Ssl.load_verify_locations
	  ctx certs.c_ca_path
	  (Filename.dirname certs.c_ca_path)

      with Invalid_argument e ->
	let error = ("Error_load_verify_locations: "^e) in
	tellserver (Exit_on_error error) ;
	exit 1
    end;

    (*
     * Extracted from the SSL_CTX_set_verify man page
     * The depth count is "level 0:peer
     * certificate", "level 1: CA certificate",
     * "level 2: higher level CA certificate", and so on.
     *
     * Setting the maximum depth to 2 allows the levels 0, 1,
     * and 2. The default depth limit is 9,
     * allowing for the peer certificate and additional 9 CA certificates. *)
    Ssl.set_verify_depth ctx 2;
    (* ********************** *)
    ctx


  method check_identity identity =
    match identity with
    | None -> None
    | Some new_remote_id ->
      try
	(* Check in the file /etc/passwd if the user new_remote_id exists *)
	Some (Unix.getpwnam new_remote_id)

      with Not_found ->
	let error =
	  ("Fatal error. User '"^new_remote_id^"' doesn't exist. \
                The network process can't take this identity")
	in
	tellserver (Types.Log (error, Error));
	failwith error


  method chroot =
    try
      match params.s_chroot with
      | None -> ()
      | Some dir ->
	Unix.chdir dir ; Unix.chroot "." ;
	tellserver
	  (Types.Log (("Network process chrooted in "^dir), Normal_Extra))

    with Unix_error (error,_,s2) ->
      let error =
	("Remote process can't chroot in '"^s2^"': "^(Unix.error_message error))
      in
      tellserver (Types.Log (error, Error));
      failwith error


  (* Change the effective uid and gid after the chroot *)
  method drop_privilegies passwd_entry_opt =
    match passwd_entry_opt with
    | None -> ()
    | Some passwd_entry ->
      setgid passwd_entry.pw_gid;
      setuid passwd_entry.pw_uid;

      match params.s_process_identity with
      | None -> assert false
      | Some new_remote_id ->
	tellserver
	  (Types.Log (("Network process identity changed to "^new_remote_id),
		      Normal_Extra))


  method initialize_network_socket =
    let port =
      match params.s_port with
      | None -> default_port
      | Some p -> p
    in

    Unix.bind sock (ADDR_INET (inet_addr_any, port));
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.listen sock backlog


  method clear_exit () =
    clients_handle#notify_all_clients RW_server_exited;
    clients_handle#disconnect_all_clients;
    Unix.shutdown sock SHUTDOWN_ALL


  method waiting_for_clients ctx =
    while true do
      let (s_cli, sockaddr_cli) = Unix.accept sock in
      let ssl_s = Ssl.embed_socket s_cli ctx in

      try
	(* Check the result of the verification of the X509 certificate
	 * presented by the peer, if any. Raises a [verify_error] on failure.
	 *)
	Ssl.verify ssl_s;
	Ssl.accept ssl_s;
	let client_connected =
	  new Ssl_connected_client.ssl_connected_client
	    ssl_s
	    sockaddr_cli
	    tellserver
	in
	ignore ( Thread.create clients_handle#new_connection
		   client_connected )

      with
      | Ssl.Verify_error _ ->
	let error = Ssl.get_error_string () in
	tellserver (Types.Log (error, Error)) ;
	prerr_endline error

      | Invalid_argument _ ->
	let error = "Error in the thread, server-side" in
	tellserver (Types.Log (error, Error)) ;
	prerr_endline error
      | Ssl.Accept_error _ ->
	let error = "A connection failed" in
	tellserver (Types.Log (error, Error)) ;
	prerr_endline error
    done


  method pipe_waits_for_notifications () =
    let bufsize = 2048 in
    let buf = String.create bufsize in

    while true do
      let recv = Unix.read tor buf 0 bufsize in
      if recv > 0 then begin
	let data = String.sub buf 0 recv in
	let notif = (Marshal.from_string data 0 : Types.notification) in
	clients_handle#notify_all_clients (Notification notif)
      end
    done


  method run =
    let ctx = self#initialize_ssl_ctx in

    (* Operations on the process itself if root *)
    if Unix.geteuid () = 0 && Unix.getegid () = 0 then begin
      (* Check needs to be done before chrooting *)
      let passwd_entry_opt =
	self#check_identity params.s_process_identity
      in
      self#chroot;
      self#drop_privilegies passwd_entry_opt;
    end;

    self#initialize_network_socket ;
    at_exit self#clear_exit;
    self#waiting_for_clients ctx;
    ignore (Thread.create self#pipe_waits_for_notifications ());
    print_endline "Waiting for connections...";


end;;
