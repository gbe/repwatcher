open Unix

let _ =

  let port = ref 9292 in
  let host = ref "" in

  let usage = "usage: rw_client host [-p port]" in

  Arg.parse
    [
      "-p", Arg.Int (fun i -> port := i), "\tPort";
    ]
    (fun s -> host := s) usage;

    if !host = "" then (Printf.printf "%s\n\n" usage; exit 1);

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

    let sock = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.connect sock sockaddr;

    while !loop do
      let data_recv = Unix.read sock buf 0 bufsize in

	if data_recv > 0 then
	  let msg = String.sub buf 0 data_recv in
	    
	    Printf.printf "Longueur: %d de '%s'\n" (String.length msg) msg;
	    String.iter (fun c -> Printf.printf "'%d'\t" (int_of_char c)) msg;
	    Pervasives.flush Pervasives.stdout;
	    
	    if msg <> "rw_exit" then
	      ignore (Unix.system ("notify-send -i /usr/share/pixmaps/nobody.png Repwatcher '"^msg^"'"))
	    else
	      loop := false
	else
	  begin
	    Printf.printf "Recu %d\n" data_recv;
	    Pervasives.flush Pervasives.stdout
	  end
    done;
    ignore (Unix.system ("notify-send -i /usr/share/pixmaps/nobody.png Repwatcher 'Server is down. Closing the client...'"));
    Unix.shutdown sock SHUTDOWN_ALL

