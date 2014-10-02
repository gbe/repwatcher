open Unix
open Types
open Files_progress

let l_encoded_chars =
(* &amp; needs to be the first one in the list.
 * Otherwise, &gt; (for example) will be changed in &amp;gt;
 *)
  let l_char = [ ("&", "&amp;")  ;
		 (">", "&gt;")   ;
		 ("<", "&lt;")   ;
		 ("'", "&apos;") ;
		 ((Char.escaped '"'), "&quot;")
	       ]
  in
  List.map (fun (char, char_encoded) ->
    ((Str.regexp char), char_encoded)
  ) l_char
;;

let escape_for_notify txt =
  List.fold_left (fun txt' (reg, encoded_char) ->
    Str.global_replace reg encoded_char txt'
  ) txt l_encoded_chars
;;


let string_of_filestate filestate =
  match filestate with
    | File_Created -> "has created"
    | File_Opened  -> "has opened"
    | File_Closed  -> "closed"
;;

let skip_because_buffered in_progress =
  Config.cfg#is_buffer_email_activated &&
    not (in_progress.ip_common.c_closing_date = None)
;;
