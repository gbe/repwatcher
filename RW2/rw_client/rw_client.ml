open Unix

let _ =

  let port = ref 9292 in
  let host = ref "" in

  let password = ref "coco" in
  let certfile = ref "cert/rw_client.crt" in
  let privkey  = ref "cert/rw_client.key" in
  let ca = "CA/CA.crt" in

  let usage = "usage: rw_client host [-p port]" in

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
      
    let () = Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_interrupt) in
    let () = Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt) in
  


      
      (try
	 while !loop do
	   let data_recv = Ssl.read ssl buf 0 bufsize in
	     
	     if data_recv > 0 then
	       let msg = String.sub buf 0 data_recv in
		 
		 if msg <> "rw_server_exit" then
		   begin
		     Printf.printf "Recu '%s'\n" msg;
		     Pervasives.flush Pervasives.stdout;
		     ignore (Unix.system (
			       "notify-send -i /usr/share/pixmaps/nobody.png Repwatcher '"^msg^"'")
			    );
		   end
		 else
		   loop := false
	     else
	       begin
		 Printf.printf "Recu %d\n" data_recv;
		 Pervasives.flush Pervasives.stdout
	       end
	 done;
       with Ssl.Read_error _ -> ()
      );
      
      Ssl.shutdown ssl;
      ignore (Unix.system ("notify-send -i /usr/share/pixmaps/nobody.png Repwatcher 'Server is down. Closing the client...'"));
      
      exit 0	   
