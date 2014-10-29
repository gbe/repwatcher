open Types
open Types_date

class email m =
object(self)
  inherit Abstract_mail.abstract_mail

  val mutable first_off_str = ""
  val mutable last_off_str = ""
  val mutable filesize_str = ""
  val mutable progression = -1.
  val filestate = Common.string_of_filestate m.m_filestate


  initializer
  let (first, last, fsize, progressionret) =
    self#_load_variables
      m.m_common.c_first_known_offset
      m.m_common.c_last_known_offset
      m.m_common.c_filesize
  in
  first_off_str <- first;
  last_off_str <- last;
  filesize_str <- fsize;
  progression <- progressionret;

  self#_set_subject ();
  self#_set_body ();


  method private _tab = self#_app "\t"
  method private _LF = self#_app "\n"

  method private _username_val =
    self#_app m.m_common.c_file.f_username

  method private _filestate_val = self#_app filestate

  method private _path = self#_app "Path:"
  method private _path_val =
    self#_app m.m_common.c_file.f_path


  method private _filename = self#_app "Filename:"

  method private _filename_val =
    self#_app m.m_common.c_file.f_name

  method private _program = self#_app "Program:"
  method private _program_val =
    self#_app m.m_common.c_file.f_program

  method private _opening_date = self#_app "Opened at:"
  method private _opening_date_val =
    self#_app (m.m_common.c_opening_date#get_str_locale)

  method private _closing_date = self#_app "Closed at:"
  method private _closing_date_val =
    let closing_date = self#_strip_option m.m_common.c_closing_date in
    self#_app (closing_date#get_str_locale)

  method private _first_known_offset = self#_app "First known offset:"
  method private _last_known_offset = self#_app "Last known offset:"
  method private _offset_val offset_str = self#_app offset_str

  method private _duration = self#_app "Duration:"
  method private _duration_val =
    let duration =
      self#_abs_duration_val
	m.m_common.c_closing_date
	m.m_common.c_opening_date
    in
    self#_app duration


  method private _progression = self#_app "Progression:"
  method private _progression_val progression =
    let progression_str = self#_string_from_progression progression in
    self#_app progression_str

  method private _size = self#_app "Size:"
  method private _size_val = self#_app filesize_str


  method private _transfer_val =
    try
      let (data_transferred_MB, percentage_transferred, transfer_rate) =
	self#_compute_transfer_rate m.m_common
      in
      let txt =
	Printf.sprintf
	  "%.02f MB transferred (%.02f%c of the file) @ %.02f KB/s"
	  data_transferred_MB
	  percentage_transferred
	  '%'
	  transfer_rate
      in
      self#_app txt
    with Abstract_mail.Not_enough_known_offsets -> self#_app ""



  method private _set_subject () =
    let file = m.m_common.c_file in
    subject <- file.f_username^" "^filestate^" "^file.f_name


  method private _set_body () =
    self#_path; self#_tab; self#_tab; self#_path_val;
    self#_LF;

    self#_filename; self#_tab; self#_tab; self#_filename_val;
    self#_LF;

    self#_program; self#_tab; self#_program_val;
    self#_LF;

    self#_opening_date; self#_tab; self#_opening_date_val;
    self#_LF;

    if m.m_filestate = File_Closed then begin

      self#_closing_date; self#_tab; self#_closing_date_val;
      self#_LF;

      self#_first_known_offset; self#_tab; self#_offset_val first_off_str;
      self#_LF;

      self#_last_known_offset; self#_tab; self#_offset_val last_off_str;
      self#_LF;

      self#_duration; self#_tab; self#_duration_val;
      self#_LF;

      self#_progression; self#_tab; self#_progression_val progression;
      self#_LF;

      self#_size; self#_tab; self#_size_val;
      self#_LF;

      self#_transfer_val
    end

end;;
