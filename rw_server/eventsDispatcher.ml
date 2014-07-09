open Inotify
open InotifyCaller
open Types

let what_to_do event =
  let (wd, tel, _, str_opt) = event in

  let name =
    match str_opt with
    | None -> "nothing"
    | Some x -> x
  in

  let rec action event_type_list is_folder =
    match event_type_list with
    | [] -> ()
    | event_type :: q ->

      if core#get_debug_event &&
	event_type <> Access then
	Printf.printf "Event in progress: '%s', %s. Name: '%s'. wd: %d\n"
	  (string_of_event event_type)
	  (string_of_bool is_folder)
	  name
	  (int_of_wd wd);

      match event_type, is_folder with
	| Isdir, _ -> action q true

	| Create, false -> FileEvents.file_created wd name
	| Open, false -> FileEvents.file_opened wd name
	| Close_write, false -> FileEvents.file_closed ~written:true wd name
	| Close_nowrite, false -> FileEvents.file_closed ~written:false wd name

	| Create, true -> core#directory_created wd name
	| Moved_from, true -> core#directory_moved_from wd name
	| Moved_to, true -> core#directory_moved_to wd name
	| Delete, true -> core#directory_deleted wd name


        (* When IGNORED is triggered it means the wd
	 * is not watched anymore. Therefore, we need
	 * to take this wd out of the Hashtbl *)
	| Ignored, _  -> ()

	(* have to be there or they're triggered by the "I don't do" *)
	| Open, true -> ()
	| Close_nowrite, true -> ()
	(* Triggered when an existing file is modified
	 * and when a file is renamed *)
	| Moved_from, false -> ()

	| _ ->
	  Log.log ("I don't do: "^(string_of_event event_type)^", "
		   ^(string_of_bool is_folder)^" yet.", Normal_Extra)

  in
  action tel false;
  Pervasives.flush Pervasives.stdout
;;
