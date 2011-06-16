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

