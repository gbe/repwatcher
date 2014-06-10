open Netsendmail
open Types
open Types_conf
open Types_date

class email m =
object(self)

  val file = m.m_file
  val filestate = Txt_operations.string_of_filestate m.m_filestate
  val mutable subject = ""
  val mutable body = ""
  val mutable first_off_str = ""
  val mutable last_off_str = ""
  val mutable filesize_str = ""
  val mutable progression = -1.

  initializer
  let (first, last, fsize) =
    match (m.m_first_offset, m.m_last_offset, m.m_filesize) with

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

	progression <- last_offset_float /. filesize_float *. 100. ;
	(Int64.to_string first_offset, Int64.to_string last_offset, Int64.to_string filesize)
  in
  first_off_str <- first;
  last_off_str <- last;
  filesize_str <- fsize;


  method private _strip_date_option date_opt =
    match date_opt with
      | None -> assert false
      | Some date -> date

  (* append to body *)
  method private _app v = body <- body^v

  method private _tab = self#_app "\t"
  method private _LF = self#_app "\n"

  method private _username_val = self#_app file.f_username

  method private _filestate_val = self#_app filestate

  method private _path = self#_app "Path:"
  method private _path_val = self#_app file.f_path

  method private _filename_val = self#_app file.f_name

  method private _program = self#_app "Program:"
  method private _program_val = self#_app file.f_program

  method private _opening_date = self#_app "Opened at:"
  method private _opening_date_val =
    let opening_date = self#_strip_date_option m.m_opening_date in
    self#_app (opening_date#get_str_locale)

  method private _closing_date = self#_app "Closed at:"
  method private _closing_date_val =
    let closing_date = self#_strip_date_option m.m_closing_date in
    self#_app (closing_date#get_str_locale)

  method private _first_known_offset = self#_app "First known offset:"
  method private _last_known_offset = self#_app "Last known offset:"
  method private _offset_val offset_str = self#_app offset_str

  method private _duration = self#_app "Duration:"
  method private _duration_val =
    let opening_date = self#_strip_date_option m.m_opening_date in
    let closing_date = self#_strip_date_option m.m_closing_date in
    let duration = closing_date#get_diff opening_date in


    (* Add the plural to txt and return the result 
     *
     * Do not return anything if value is 0
     * unless print_anyway is true *)
    let plural ?(print_anyway=false) txt value =
      let singular = Printf.sprintf "%d %s" value txt in
      if value == 1 then
	singular^" "
      else if value >= 2 then
	singular^"s "
      else
	begin
	  if print_anyway then
	    singular
	  else ""
	end
    in

    (* Not appended if equal to 0 *)
    self#_app (plural "year" duration.years);
    self#_app (plural "month" duration.months);
    self#_app (plural "day" duration.days);
    self#_app (plural "hour" duration.hours);
    (* ************************** *)

    self#_app (plural ~print_anyway:true "minute" duration.minutes);
    self#_app (plural ~print_anyway:true "second" duration.seconds);


  method private _progression = self#_app "Progression:"
  method private _progression_val =
    if progression <= 0. then
      self#_app "N/A"
    else
      self#_app (Printf.sprintf "%.02f%c" progression '%')


  method private _size = self#_app "Size:"
  method private _size_val = self#_app filesize_str

  method private _transfer_val =

    (* Bandwidth computation added to string *)
    match (m.m_first_offset, m.m_last_offset) with
      | Some first, Some last ->
	let opening_date = self#_strip_date_option m.m_opening_date in
	let closing_date = self#_strip_date_option m.m_closing_date in

	let data_transferred = Int64.to_float (Int64.sub last first) in
	(* 1MB = 1048576 = 1024 * 1024 *)
	let data_transferred_MB = data_transferred /. 1048576. in
	let percentage_transferred = data_transferred /. (float_of_string filesize_str) *. 100. in
	let bandwidth =
	  data_transferred /. (closing_date#get_diff_sec opening_date) /. 1024.
	in	

	let txt =
	  Printf.sprintf
	    "%.02f MB transferred (%.02f%c of the file) @ %.02f KB/s"
	    data_transferred_MB
	    percentage_transferred
	    '%'
	    bandwidth
	in
	self#_app txt

      | _ -> ()


  method set_subject =
    subject <- file.f_username^" "^filestate^" "^file.f_name


  method set_body =
    self#_path; self#_tab; self#_tab; self#_path_val;
    self#_LF;

    self#_program; self#_tab; self#_program_val;
    self#_opening_date; self#_tab; self#_opening_date_val;

    if m.m_filestate = File_Closed then begin

      self#_tab; self#_tab;
      self#_first_known_offset; self#_tab; self#_offset_val first_off_str;
      self#_LF;

      self#_closing_date; self#_tab; self#_closing_date_val;
      self#_tab;
      self#_tab;
      self#_last_known_offset; self#_tab; self#_offset_val last_off_str;
      self#_duration; self#_tab; self#_duration_val;
      self#_tab;
      self#_tab;
      self#_progression; self#_tab; self#_progression_val;
      self#_LF;

      self#_size;
      self#_tab;
      self#_size_val;

      self#_LF;
      self#_transfer_val
    end

  method get_body = body
  method get_subject = subject
  method get_filestate = filestate

end;;


let send mail =

  let file = mail.m_file in

  try
    let e_conf = (Config.cfg)#get_email in

    let email = new email mail in
    email#set_subject;
    email#set_body;
  
    (***** Let's log it *****)
    List.iter (fun recipient ->
      let txt2log = "Sending email to "^recipient^" about "^file.f_username^" who "^email#get_filestate^" "^file.f_name in

      Log.log (txt2log, Normal)
    ) e_conf.e_recipients;
  (************************)


    Sendmail.sendmail e_conf email#get_subject email#get_body ()

  (* Should not happen as it is filtered in report.ml *)
  with Config.Email_not_configured ->
    Log.log
      ("Tried to prepare an email while the emails are disabled",
       Error);
    assert false
;;
