open Netsendmail
open Types
open Types_conf

class virtual abstract_mail =
object(self)

  val mutable body = ""
  val mutable subject = ""

  method virtual private _set_subject : unit -> unit
  method virtual private _set_body : unit -> unit

  method private _strip_option : 'a. 'a option -> 'a =
    fun arg ->
      match arg with
      | None -> assert false
      | Some b -> b


  (* append to body *)
  method private _app v = body <- body^v


  method send ?(html=false) () =
    let email_conf = Config.cfg#get_email in
(*    let m = self#_strip_option m_opt in
    let file = m.m_common.c_file in

    (***** Let's log it *****)
    List.iter (fun recipient ->
      let txt2log =
	"Sending email to "^recipient^" about "^file.f_username^
	  " who "^filestate^" "^file.f_name
      in

      Log.log (txt2log, Normal)
    ) email_conf.e_recipients;
    (************************)
*)
    Sendmail.sendmail html email_conf subject body ()

end;;
