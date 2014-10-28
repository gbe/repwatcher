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

  method private _action_to_string written close_date =
    match (written, close_date) with
    | (false, None) -> "has accessed"
    | (false, Some _) -> "accessed"
    | (true, None) -> "is creating"
    | (true, Some _) -> "created"

  method private _set_html_headers =
    let headers = "<html><head><title>Repwatcher Events</title>" in

    (* Clear the body content since a new mail is building*)
    body <- headers

  method private _set_html_footer =
    self#_app "</html>"

  method private _set_subject () =
    subject <- Printf.sprintf "Repwatcher - events buffer"

  method private _set_css =
    let css ="<style>

body {
	font-family: verdana,helvetica,arial,sans-serif;
	font-size: 14px;
}

table {
	border-spacing: 2px;
	border-collapse: collapse;
	width: 100%;
}

thead {
	color: #ffffff;
	background-color: #555555;
	border: 1px solid #555555;
	padding: 3px;
	vertical-align: top;
	text-align: left;
	font-weight: bold;
	vertical-align:middle;
}

td {
	border:1px solid gray ;
	vertical-align:middle;
	padding: 5px;
}

tbody tr:nth-child(odd) {
	background-color: #f1f1f1;
}
</style>" in
    self#_app css

  method private _add_column ?(theader=false) text =
    match theader with
    | false -> self#_app ("<td>"^text^"</td>")
    | true -> self#_app ("<th>"^text^"</th>")

  method private _add_table_header =
    self#_app "<thead><tr>";
    List.iter (fun colName ->
      self#_add_column
	~theader:true
	colName)
      ["Username";
       "Action";
       "Path";
       "Filename";
       "Program";
       "Filesize";
       "First Known Offset";
       "Last Known Offset";
       "Opening date";
       "Closing date";
       "Duration";
       "Progression";
       "Sum up";
      ];
    self#_app "</tr></thead>"


  method private _add_row (wd, file) ip =
    let c = ip.ip_common in

    let offset_to_string offset_opt =
      match offset_opt with
      | None -> ""
      | Some offset -> Int64.to_string offset
    in

    self#_app "<tr>";

    List.iter
      self#_add_column
      [file.f_username;
       self#_action_to_string c.c_written c.c_closing_date;
       file.f_path;
       file.f_name;
       file.f_program;
       (match c.c_filesize with
       | None -> assert false
       | Some filesize -> Int64.to_string filesize);
       offset_to_string c.c_first_known_offset;
       offset_to_string c.c_last_known_offset;
       c.c_opening_date#get_str_locale;
       (match c.c_closing_date with
       | None -> ""
       | Some date -> date#get_str_locale);
       self#_abs_duration_val c.c_closing_date c.c_opening_date;
       "";
       "";
      (*       "Progression"; *)
      (*        "Sum up"; *)

      ];
    self#_app "</tr>";


  method private _set_body () =
    self#_app "</head><body><table>";
    self#_add_table_header;
    self#_app "<tbody>";
    Hashtbl.iter (fun key in_progress ->
      self#_add_row key in_progress
    ) Files_progress.htbuffer;
    self#_app "</tbody></table></body>"

  method start_running () =
    let waiting_time =
      float_of_int Config.cfg#get_email_buffer
    in

    while true do
     (* Thread.delay waiting_time ; *)
      Thread.delay 30.0 ;

      if Hashtbl.length Files_progress.htbuffer > 0 then
	begin
	  self#_set_subject ();
	  self#_set_html_headers;
	  self#_set_css;
	  self#_set_body ();
	  self#_set_html_footer;

	  self#send ~html:true ();
	  Files_progress.remove_closed_files ();
	end
    done

end;;

let bMail = new bufferMail ;;
