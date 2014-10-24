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
    (* Clear the body content since a new mail is building*)
    body <- "";

    let headers = "<html><title>Repwatcher Events</title>" in
    self#_app headers

  method private _set_html_footer =
    self#_app "</html>"

  method private _set_subject () =
    subject <- Printf.sprintf "Repwatcher - events buffer"

  method private _add_column text =
    self#_app ("<td>"^text^"</td>")

  method private _add_table_header =
    self#_app "<tr>";
    List.iter
      self#_add_column
      ["Username";
       "Path";
       "Filename";
       "Filesize";
       "Opening date";
       "Closing date"
      ];
    self#_app "</tr>"


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
    self#_app "<body><table border=2>";
    self#_add_table_header;
    Hashtbl.iter (fun key in_progress ->
      self#_add_row key in_progress
    ) Files_progress.ht;
    self#_app "</table></body>"

  method start_running () =
    let waiting_time =
      float_of_int Config.cfg#get_email_buffer
    in

    while true do
(*      Thread.delay waiting_time ; *)
      Thread.delay 10.0;
      if Hashtbl.length Files_progress.ht > 0 then
	begin
	  Log.log ("Buffer mail hello", Normal_Extra);
	  Pervasives.flush Pervasives.stdout;
	  self#_set_subject ();
	  self#_set_html_headers;
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
