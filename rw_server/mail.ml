open Netsendmail ;;
open Types ;;
open Types_conf ;;


let send mail =

  let file = mail.m_file in
  let conf = (Config.get ()).c_email in

  let filestate_str =
    match mail.m_filestate with
      | File_Created -> "has created"
      | File_Opened  -> "has opened"
      | File_Closed  -> "closed"
  in

  let gentxt () =

    let txt =
      ref (Printf.sprintf "%s %s %s\n\nPath: %s\nProgram: %s" file.f2_username filestate_str file.f2_name file.f2_path file.f2_program)
    in

    if mail.m_filestate = File_Closed then begin

      let opening_date =
	match mail.m_opening_date with
	  | None -> assert false
	  | Some date -> date
      in

      let progression_float = ref (-1.) in

      let (off_str, filesize_str) =
	match (mail.m_offset, mail.m_filesize) with
	| (None, None) -> ("Unknown", "Unknown")
	| (None, Some filesize) -> ("Unknown", Int64.to_string filesize)
	| (Some offset, None) -> (Int64.to_string offset, "Unknown")
	| (Some offset, Some filesize) ->
	  let off_float = Int64.to_float offset in
	  let filesize_float = Int64.to_float filesize in
	  progression_float := off_float/.filesize_float*.100.;
	  (Int64.to_string offset, Int64.to_string filesize)
      in

      let progression =
	if !progression_float <= 0. then
	  "N/A"
	else
	  Printf.sprintf "%.02f%c" !progression_float '%'
      in
      
      txt := Printf.sprintf "%s\nOpened: %s\nProgression: %s\tLast Known Offset: %s\tSize: %s" (!txt) opening_date progression off_str filesize_str
    end;

    !txt
  in




  (* This avoids to ask for the recipients's real name in the config file *)
  let recipients =
    List.map (fun address ->
      ("", address))
      conf.e_recipients
  in

  let subject = file.f2_username^" "^filestate_str^" "^file.f2_name in
  
  (***** Let's log it *****)
  List.iter (fun recipient ->
    let txt2log = "Sending email to "^recipient^" about "^file.f2_username^" who "^filestate_str^" "^file.f2_name in

    Log.log (txt2log, Normal_Extra)
  ) conf.e_recipients;
  (************************)

  let m =
    compose ~from_addr:(conf.e_sender_name, conf.e_sender_address) ~to_addrs:recipients ~subject:subject ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8 (gentxt ())
  in
  sendmail ~mailer:"sendmail" m
;;
