open Netsendmail
open Types
open Types_conf
open Types_date

exception Not_enough_known_offsets

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


  method private _load_variables
    first_known_offset_opt
    last_known_offset_opt
    filesize_opt =

    let progression = ref (-1.) in

    let (first, last, fsize) =
      match (first_known_offset_opt, last_known_offset_opt, filesize_opt) with

      (* If first_offset is known, last_offset is a None as well.
       * To avoid writing the unnecessary cases, I put a _ *)
      | (None, _, None) -> ("Unknown", "Unknown", "Unknown")

      | (None, _, Some filesize) -> ("Unknown", "Unknown", Int64.to_string filesize)

      (* Last_known_offset cannot be a None if First_known_offset is not *)
      | (Some first_offset, None, _) -> assert false

      | (Some first_offset, Some last_offset, None) ->
	(Int64.to_string first_offset, Int64.to_string last_offset, "Unknown")

      | (Some first_offset, Some last_offset, Some filesize) ->
	let last_offset_float = Int64.to_float last_offset in
	let filesize_float = Int64.to_float filesize in

	progression := last_offset_float /. filesize_float *. 100. ;
	(Int64.to_string first_offset, Int64.to_string last_offset, Int64.to_string filesize)
    in
    (first, last, fsize, !progression)


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


  method private _string_from_progression progression =
    if progression <= 0. then
      "Unknown"
    else
      Printf.sprintf "%.02f%c" progression '%'


  method private _compute_transfer_rate common =

    match (common.c_first_known_offset, common.c_last_known_offset) with
    | Some first, Some last ->

      (* If the file is still opened, creates a new date *)
      let date =
	match common.c_closing_date with
	| None -> new Date.date
	| Some closing_date -> closing_date
      in

      let data_transferred = Int64.to_float (Int64.sub last first) in
      (* 1MB = 1048576 = 1024 * 1024 *)
      let data_transferred_MB = data_transferred /. 1048576. in
      let filesize = Int64.to_float (self#_strip_option common.c_filesize) in
      let percentage_transferred = data_transferred /. filesize *. 100. in
      let transfer_rate =
	data_transferred /. (date#get_diff_sec common.c_opening_date) /. 1024.
      in

      (data_transferred_MB, percentage_transferred, transfer_rate)

    | _ -> raise Not_enough_known_offsets

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
