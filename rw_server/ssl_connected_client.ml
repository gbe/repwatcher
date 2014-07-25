open Unix
open Types

class ssl_connected_client ssl_s sockaddr_cli tellserver =

  let init_subj = Ssl.get_subject (Ssl.get_certificate ssl_s) in
  let init_common_name =
    let pat = Str.regexp "/" in
    let cn = Str.regexp "CN=" in

    let rec loop = function
      | [] -> "Unknown user"
      | h :: q ->
	if Str.string_match cn h 0 then
	  let lastpos = Str.match_end () in
	  String.sub h lastpos ((String.length h)-lastpos)
	else
	  loop q
    in
    loop (Str.split pat init_subj)
  in

  (* Return the IP from the socket *)
  let init_ip =
    let inet_addr_of_sockaddr = function
      | Unix.ADDR_INET (n, _) -> n
      | Unix.ADDR_UNIX _ -> Unix.inet_addr_any
    in
    let inet_addr = inet_addr_of_sockaddr sockaddr_cli  in
    Unix.string_of_inet_addr inet_addr
  in
object(self)

  val subj = init_subj
  val cipher_name = Ssl.get_cipher_name (Ssl.get_cipher ssl_s)
  val common_name = init_common_name
  val ip = init_ip


  (* Send the notification to one or several clients *)
  method send_notification (com : com_server2clients) =
    let ser_com = Marshal.to_string com [Marshal.No_sharing] in

    try
      Ssl.output_string ssl_s ser_com
    with Ssl.Write_error _ ->
      tellserver (Types.Log ("SSL write error", Error))


    (* If sock is not given then it means the
     * notification must be sent to every one *)
    (*match sock_opt with
    | None ->
      Mutex.lock m ;
      List.iter do_it !connected_clients;
      Mutex.unlock m
    | Some sock' -> do_it sock'
    *)

  method get_ssl_s = ssl_s
  method get_common_name = common_name
  method get_ip = ip
  method get_cipher_name = cipher_name

end ;;
