open Netsendmail ;;
open Types ;;
open Types_conf ;;
open Types_date ;;


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
      ref (Printf.sprintf "%s %s %s\n\nPath: %s\nProgram: %s\nOpened on: %s"
	     file.f2_username
	     filestate_str
	     file.f2_name
	     file.f2_path
	     file.f2_program
	     opening_date#get_str_locale)
    in

    if mail.m_filestate = File_Closed then begin
      let closing_date =
	match mail.m_closing_date with
	| None -> assert false
	| Some date -> date
      in
      let progression_float = ref (-1.) in

      let (first_off_str, last_off_str, filesize_str) =
	match (mail.m_first_offset, mail.m_last_offset, mail.m_filesize) with

	(* If first_offset is known, last_offset is a None as well.
	 * To avoid writing the unnecessary cases, I put a _ *)
	| (None, _, None) -> ("Unknown", "Unknown", "Unknown")
	| (None, _, Some filesize) -> ("Unknown", "Unknown", Int64.to_string filesize)

	  (* Last_known_offset cannot be a None if First_known_offset is not*)
	| (Some first_offset, None, _) -> assert false

	| (Some first_offset, Some last_offset, None) -> 
	  (Int64.to_string first_offset, Int64.to_string last_offset, "Unknown")

	| (Some first_offset, Some last_offset, Some filesize) ->
	  let last_offset_float = Int64.to_float last_offset in
	  let filesize_float = Int64.to_float filesize in
	  progression_float := last_offset_float /. filesize_float *. 100. ;

	  (Int64.to_string first_offset, Int64.to_string last_offset, Int64.to_string filesize)
      in

      let progression =
	if !progression_float <= 0. then
	  "N/A"
	else
	  Printf.sprintf "%.02f%c" !progression_float '%'
      in


      let duration = closing_date#get_diff opening_date in
      let duration_txt = ref "" in
      if duration.years > 0 then
	duration_txt := Printf.sprintf "%d years" duration.years;

      if duration.months > 0 then
	duration_txt := Printf.sprintf "%s %d months" !duration_txt duration.months;

      if duration.days > 0 then
	duration_txt := Printf.sprintf "%s %d days" !duration_txt duration.days;

      if duration.hours > 0 then
	duration_txt := Printf.sprintf "%s %d hours" !duration_txt duration.hours;

      duration_txt :=
	Printf.sprintf "%s %d minutes %d seconds"
	!duration_txt
	duration.minutes
	duration.seconds;


      txt :=
	Printf.sprintf
	"%s\tFirst known offset: %s\nClosed on: %s\tLast known offset: %s\nDuration: %s\tProgression: %s\nSize: %s"
	(!txt)
	first_off_str
	closing_date#get_str_locale
	last_off_str
	(!duration_txt)
	progression
	filesize_str ;

      (* Bandwidth computation added to string *)
      match (mail.m_first_offset, mail.m_last_offset) with
      | Some first, Some last ->
	Printf.printf "first: %Ld\tlast: %Ld\n" first last;
	let bandwidth =
	  (Int64.to_float last) -. (Int64.to_float first) /.
	    (closing_date#get_diff_sec opening_date) /. 1024.
	in
	Printf.printf "bandwidth: %.02f\n" bandwidth;
	txt := Printf.sprintf "%s\nBandwidth rate: %.02f KB/s" (!txt) bandwidth;
      | _ -> ()
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
