open Types
open Types_conf
open Files_progress

(* In the last 30 minutes...

   File1_name has been opened.
   File2_name has been opened and closed.

*)

class bufferMail =
object(self)
  inherit Abstract_mail.abstract_mail

  method private _set_html_headers =
    let headers = "<html><title>Repwatcher Events</title>" in
    self#_app headers

  method private _set_html_footer =
    self#_app "</html>"

  method private _set_subject () =
    subject <- Printf.sprintf "Repwatcher - events buffer"

  method private _add_row (wd, file) ip =
    let txt =
      Printf.sprintf "<tr><td>%s</td><td>%s</td></tr>" file.f_path file.f_name
    in
    self#_app txt

  method private _set_body () =
    self#_app "<body><table>";
    Hashtbl.iter self#_add_row Files_progress.ht;
    self#_app "</table></body>"

  method start_running () =
    let waiting_time =
      float_of_int Config.cfg#get_email_buffer
    in

    while true do
(*      Thread.delay waiting_time ; *)
      Thread.delay 10.0;
      print_endline "Buffer mail hello";
      Pervasives.flush Pervasives.stdout;
      self#_set_subject ();
      self#_set_html_headers;
      self#_set_body ();
      self#_set_html_footer;
      print_endline ("Subject: "^subject);
      print_endline ("Body: "^body);
      self#send;
    done

end;;

let bMail = new bufferMail ;;
