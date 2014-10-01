open Types
open Types_conf

(* In the last 30 minutes...

   File1_name has been opened.
   File2_name has been opened and closed.

*)

class bufferMail =
object(self)
  inherit Abstract_mail.abstract_mail

  method start_running () =
    let waiting_time =
      float_of_int Config.cfg#get_email_buffer
    in

    while true do
      print_endline "Buffer mail hello";
      Pervasives.flush Pervasives.stdout;
      Thread.delay waiting_time ;
    done

end;;

let bMail = new bufferMail ;;
