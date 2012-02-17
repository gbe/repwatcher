open Netsendmail;;
open Types;;


let send (filestate, file) =
  let filestate_str =
    match filestate with
      | File_Created -> "has created"
      | File_Opened  -> "has opened"
      | File_Closed  -> "closed"
  in

  let txt =
    Printf.sprintf "%s %s %s\nPath: %s\nProgram: %s" file.f2_username filestate_str file.f2_name file.f2_path file.f2_program
  in

  let m =
    compose ~from_addr:("Repwatcher","repwatcher@gatekeeper.fr") ~to_addrs:[("Gregory Bellier", "gregory.bellier@gmail.com")] ~subject:"Repwatcher event" txt
  in
  sendmail ~mailer:"sendmail" m;
  Nothing
;;
