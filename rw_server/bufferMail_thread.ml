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
       "Path";
       "Filename";
       "Filesize";
       "Opening date";
       "Closing date"
      ];
    self#_app "</tr></thead>"


  method private _add_row (wd, file) ip =
    self#_app "<tr>";
    List.iter
      self#_add_column
      [file.f_username;
       file.f_path;
       file.f_name;
       (match ip.ip_common.c_filesize with
       | None -> assert false
       | Some filesize -> Int64.to_string filesize);
       ip.ip_common.c_opening_date#get_str_locale;
       (match ip.ip_common.c_closing_date with
       | None -> ""
       | Some date -> date#get_str_locale)
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
(*      Thread.delay waiting_time ; *)
      Thread.delay 30.0;
      if Hashtbl.length Files_progress.htbuffer > 0 then
	begin
	  Log.log ("Buffer mail hello", Normal_Extra);

	  self#_set_subject ();
	  self#_set_html_headers;
	  self#_set_css;
	  self#_set_body ();
	  self#_set_html_footer;
	  print_endline ("Subject: "^subject);
	  print_endline ("Body: "^body);
	  self#send;
	  Files_progress.remove_closed_files ();
	end
    done

end;;

let bMail = new bufferMail ;;
