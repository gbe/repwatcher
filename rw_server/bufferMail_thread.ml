open Types
open Types_conf
open Files_progress


class bufferMail =
object(self)
  inherit Abstract_mail.abstract_mail

  method private _action_to_string written close_date =
    match (written, close_date) with
    | (false, None) -> "has accessed"
    | (false, Some _) -> "closed"
    | (true, None) -> "is creating"
    | (true, Some _) -> "created"

  method private _set_html_headers =
    let headers = "<html><head><title>Repwatcher Events</title>" in

    (* Clear the body content since a new mail is being created *)
    body <- headers

  method private _set_html_footer =
    self#_app "</html>"

  method private _set_subject () =
    subject <- "Repwatcher events"

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
       "Program";
       "Action";
       "Path";
       "Filename";
       "Filesize";
       "Data transferred";
       "Transfer rate";
       "Overall progression";
       "Opening date";
       "Closing date";
       "Duration";
      ];
    self#_app "</tr></thead>"


  method private _add_row (wd, file) ip =
    let c = ip.ip_common in

    let (first_known_offset_str,
	 last_known_offset_str,
	 filesize_str,
	 progression) =
      self#_load_variables
	c.c_first_known_offset
	c.c_last_known_offset
	c.c_filesize
    in

    let (data_transferred_column, transfer_rate_column) =
      try
	let (data_transferred_MB, percentage_transferred, transfer_rate) =
	  self#_compute_transfer_rate ip.ip_common
	in

	let dt_col =
	  Printf.sprintf "%.02f MB<br />(%.02f%c of the file)"
	    data_transferred_MB
	    percentage_transferred
	    '%'
	in
	let tr_col =
	  Printf.sprintf "%.02f KB/s" transfer_rate
	in
	(dt_col, tr_col)
      with
	Abstract_mail.Not_enough_known_offsets -> ("", "")
    in


    self#_app "<tr>";

    List.iter
      self#_add_column
      [
	file.f_username;
	file.f_program;
	self#_action_to_string c.c_written c.c_closing_date;
	file.f_path;
	file.f_name;
	filesize_str;
	data_transferred_column;
	transfer_rate_column;
	self#_string_from_progression progression;
	c.c_opening_date#get_str_locale;
	(match c.c_closing_date with
	| None -> ""
	| Some date -> date#get_str_locale);
	self#_abs_duration_val c.c_closing_date c.c_opening_date;
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
      Thread.delay waiting_time ;

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
