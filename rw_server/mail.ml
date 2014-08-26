open Types

class email m =
object(self)
  inherit Abstract_mail.abstract_mail


  initializer
    m_opt <- Some m;
    self#_init_variables;
    self#_set_subject;
    self#_set_body;


  method private _set_subject =
    let file = m.m_common.c_file in
    subject <- file.f_username^" "^filestate^" "^file.f_name


  method private _set_body =
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

end;;
