open Netsendmail
open Types
open Types_conf
open Types_date

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

  method private _abs_duration_val enddate startdate =
    let enddate_o =
      match enddate with
      | None -> new Date.date
      | Some closing_date -> closing_date
    in

    let duration = enddate_o#get_diff startdate in


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
	    singular^" "
	  else ""
	end
    in

    let str_duration = ref "" in
    (* Not appended if equals to 0 *)
    str_duration := !str_duration^(plural "year" duration.years);
    str_duration := !str_duration^(plural "month" duration.months);
    str_duration := !str_duration^(plural "day" duration.days);
    str_duration := !str_duration^(plural "hour" duration.hours);
    (* ************************** *)

    str_duration := !str_duration^(plural ~print_anyway:true "minute" duration.minutes);
    str_duration := !str_duration^(plural ~print_anyway:true "second" duration.seconds);
    !str_duration


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
