open Netsendmail ;;
open Types ;;
open Types_conf ;;


let send (filestate, file, offset_opt, filesize_opt) =

  let conf = (Config.get ()).c_email in  

  let filestate_str =
    match filestate with
      | File_Created -> "has created"
      | File_Opened  -> "has opened"
      | File_Closed  -> "closed"
  in

  let gentxt () =

    let txt =
      ref (Printf.sprintf "%s %s %s\nPath: %s\nProgram: %s" file.f2_username filestate_str file.f2_name file.f2_path file.f2_program)
    in

    if filestate = File_Closed then begin
      let progression_float = ref (-1.) in

      let (off_str, filesize_str) =
	match (offset_opt, filesize_opt) with
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
	if !progression_float == -1. then
	  "N/A"
	else
	  Printf.sprintf "%.02f" !progression_float
      in
      
      txt := Printf.sprintf "%s\nProgression: %s%c\nLast Known Offset: %s\nSize: %s" (!txt) progression '%' off_str filesize_str
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

  let m =
    compose ~from_addr:(conf.e_sender_name, conf.e_sender_address) ~to_addrs:recipients ~subject:subject ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8 (gentxt ())
  in
  sendmail ~mailer:"sendmail" m
;;
