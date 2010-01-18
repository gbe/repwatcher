open Unix

let port         = ref 9292
let backlog      = 15



let run tor =

  let connected_clients = ref [] in

  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.bind sock (ADDR_INET (inet_addr_any, !port));
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.listen sock backlog;


  let send txt sock =
    
    let do_it sock =
      let sent = Unix.send sock txt 0 (String.length txt) [MSG_PEEK] in
	Printf.printf "Sent '%s' : %d\n" txt sent;
	Pervasives.flush Pervasives.stdout
    in

    match sock with
      | None   -> List.iter (fun (s_cli, _ ) -> do_it s_cli ) !connected_clients
      | Some s -> do_it s
  in


  let handle_interrupt i = 
    send "rw_exit" None;
    exit 0
  in




  let wait_for_pipe () =
    let bufsize = 1024 in
    let buf = String.create bufsize in
    let loop = ref true in

    while !loop do
      let recv = Unix.read tor buf 0 bufsize in
	if recv > 0 then
	  begin
	    let msg = String.sub buf 0 recv in
	    send msg None
	  end
    done
  in


  let handle_connection caller =

    let loop = ref true in
    Printf.printf "New connection\n";

    let s_cli,sockaddr_cli = caller in

    let inet_addr_of_sockaddr = function
      | Unix.ADDR_INET (n, _) -> n
      | Unix.ADDR_UNIX _ -> Unix.inet_addr_any
    in
    let inet_addr = inet_addr_of_sockaddr sockaddr_cli  in
    let ip = Unix.string_of_inet_addr inet_addr in

    Printf.printf "Welcome %s\n" ip;

    connected_clients := (s_cli, sockaddr_cli) :: !connected_clients;
    send "Successfully connected" (Some (s_cli));
    Pervasives.flush Pervasives.stdout;
    (*while !loop do
      Thread.yield();
      Printf.printf "Après le wait_read\n";
    done*)

  in


  let () = Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_interrupt) in
  let () = Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt) in

  
  ignore (Thread.create wait_for_pipe ());

  Printf.printf "Waiting for connections...\n";
  Pervasives.flush Pervasives.stdout;

  while true do
    let caller = Unix.accept sock in
      ignore (Thread.create handle_connection caller);
  done
  ;
  exit 0
