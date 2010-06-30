(*
    This file is part of Repwatcher.

    Repwatcher is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Repwatcher is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Repwatcher; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*)



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

