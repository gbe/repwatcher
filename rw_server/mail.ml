open Netsendmail
open Types
open Types_conf

class email m =
object(self)
  inherit Abstract_mail.abstract_mail


  initializer
    m_opt <- Some m;
    self#_init_variables


  method set_subject =
    let file = m.m_common.c_file in
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

  let file = mail.m_common.c_file in

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
