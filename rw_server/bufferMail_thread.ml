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

  method private _set_subject () = ()

  method private _add_row (wd, file) ip =
    ()

  method private _set_body () =
    Hashtbl.iter self#_add_row Files_progress.ht

  method start_running () =
    let waiting_time =
      float_of_int Config.cfg#get_email_buffer
    in

    while true do
      Thread.delay waiting_time ;
      print_endline "Buffer mail hello";
      Pervasives.flush Pervasives.stdout;
      self#_set_subject ();
      self#_set_body ();
    done

end;;

let bMail = new bufferMail ;;
