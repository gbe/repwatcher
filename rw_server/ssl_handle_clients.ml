open Unix
open Types_conf
open Types
open Printf

class ssl_handling_clients tellserver =
object(self)

  val m = Mutex.create ()
  val mutable connected_clients = []

  (* Remove the client from the connected clients list *)
  (* TODO: remove sock_cli and do the partition directly on the client
     object *)
  method client_quit sock_cli =
    Mutex.lock m ;
    let (l_clients_left, l_client) =
      List.partition (fun (client, _) ->
	client#get_ssl_s != sock_cli
      ) connected_clients
    in
    connected_clients <- l_clients_left ;
    Mutex.unlock m ;

    let (client, _) = List.hd l_client in
    let log_msg = sprintf
      "%s has quit (%s)"
      client#get_common_name
      client#get_ip
    in
    print_endline log_msg;
    tellserver ( Types.Log (log_msg, Normal) );
    Ssl.shutdown sock_cli


  method disconnect_all_clients =
    Mutex.lock m ;
    (* Close the clients' sockets *)
    List.iter (fun (client, _) ->
      Ssl.flush client#get_ssl_s ;
      Ssl.shutdown client#get_ssl_s
    ) connected_clients;
    Mutex.unlock m


  method notify_new_clients_with_current_accesses com =
    Mutex.lock m;
    let updated_connected_clients =
      List.fold_left (fun acc (client, has_already_received_current_accesses) ->
	if has_already_received_current_accesses = false then
	  self#notify client com ;

	(client, true) :: acc
      ) [] connected_clients
    in
    connected_clients <- updated_connected_clients;
    Mutex.unlock m

  method notify (client : Ssl_connected_client.ssl_connected_client) com =
    (* Resend the data already serialized to the clients *)
    match com with
    | (RW_server_exited | RW_server_con_ok _) ->
      client#send_notification com
    | Notification notif ->
      match notif with
      | Local_notif _ -> assert false
      | New_notif _ -> client#send_notification com
      | Old_notif _ -> client#send_notification com

  method notify_all_clients com =
    Mutex.lock m;
    List.iter (fun (client, _) -> self#notify client com) connected_clients;
    Mutex.unlock m


(* This is where a new connection is processed *)
(* This function runs in a dedicated thread *)
  method new_connection (client : Ssl_connected_client.ssl_connected_client) =

    let new_client_txt =
      sprintf "%s has connected from %s (using: %s)"
	client#get_common_name
	client#get_ip
	client#get_cipher_name
    in
    print_endline new_client_txt;
    tellserver (Types.Log (new_client_txt, Normal)) ;

    Mutex.lock m ;
    (* the 'false' here is to keep in memory
     * that the new client has not received the
     * currents accesses yet *)
    connected_clients <- (client, false) :: connected_clients;
    Mutex.unlock m ;

    (* To be moved from there *)
    let conf = (Config.cfg)#get in

    (* Tell the new client that he is authorized
     * and send him at the same time the number
     * of last folders to display set in the config file *)
    client#send_notification
      (RW_server_con_ok conf.c_notify.n_parent_folders);

    (* Ask father's process for the current accesses
     * to send them to the new client *)
    tellserver Ask_current_accesses;
    let loop = ref true in

    try
      (* Wait for client exit *)
      while !loop do
	let msg = Ssl.input_string client#get_ssl_s in

	if msg = "rw_client_exit" then begin
	  self#client_quit client#get_ssl_s;
	  loop := false
	end
      done
    with Ssl.Read_error _ -> self#client_quit client#get_ssl_s

end;;
