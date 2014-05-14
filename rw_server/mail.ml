open Netsendmail ;;
open Types ;;
open Types_conf ;;


let send mail =

  let file = mail.m_file in
  let conf_e_opt = ((Config.cfg)#get).c_email in

  let e =
    match conf_e_opt with
    (* Cannot happen as it is filtered in report.ml *)
      | None -> assert false
      | Some e -> e
  in

  let filestate_str =
    match mail.m_filestate with
      | File_Created -> "has created"
      | File_Opened  -> "has opened"
      | File_Closed  -> "closed"
  in

  let opening_date =
    match mail.m_opening_date with
    | None -> assert false
    | Some date -> date
  in

  let gentxt () =

    let txt =
      ref (Printf.sprintf "%s %s %s\n\nPath: %s\nProgram: %s\nOpened: %s"
	     file.f2_username
	     filestate_str
	     file.f2_name
	     file.f2_path
	     file.f2_program
	     opening_date)
    in

    if mail.m_filestate = File_Closed then begin
      let closing_date =
	match mail.m_closing_date with
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
      
      txt :=
	Printf.sprintf
	"%s\nClosed: %s\nProgression: %s\tLast Known Offset: %s\tSize: %s"
	(!txt)
	closing_date
	progression
	off_str
	filesize_str
    end;

    !txt
  in


  let subject = file.f2_username^" "^filestate_str^" "^file.f2_name in
  
  (***** Let's log it *****)
  List.iter (fun recipient ->
    let txt2log = "Sending email to "^recipient^" about "^file.f2_username^" who "^filestate_str^" "^file.f2_name in

    Log.log (txt2log, Normal)
  ) e.e_recipients;
  (************************)

  Sendmail.sendmail e subject (gentxt ()) ()
;;
