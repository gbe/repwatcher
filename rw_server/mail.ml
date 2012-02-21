open Netsendmail ;;
open Types ;;
open Types_conf ;;


let send (filestate, file) =

  let conf = (Config.get ()).c_email in  

  let filestate_str =
    match filestate with
      | File_Created -> "has created"
      | File_Opened  -> "has opened"
      | File_Closed  -> "closed"
  in

  let txt =
    Printf.sprintf "%s %s %s\nPath: %s\nProgram: %s" file.f2_username filestate_str file.f2_name file.f2_path file.f2_program
  in

  (* This avoids to ask for the recipients's real name in the config file *)
  let recipients =
    List.map (fun address ->
      ("", address))
      conf.e_recipients
  in

  let m =
    compose ~from_addr:(conf.e_sender_name, conf.e_sender_address) ~to_addrs:recipients ~subject:"Repwatcher event" txt
  in
  sendmail ~mailer:"sendmail" m
;;
