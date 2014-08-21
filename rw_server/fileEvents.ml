open Printf
open Unix
open Types
open InotifyCaller
open Report
open Sql_report
open Files_progress

let print_file file =
  Printf.printf
    "Name: %s\n\
     Path: %s\n\
     Login: %s\n\
     Username: %s\n\
     Program: %s (pid: %d)\n\
     Descr: %d\n\n"
      file.f_name
      file.f_path
      file.f_unix_login
      file.f_username
      file.f_program
      (Fdinfo.int_of_pid file.f_program_pid)
      (Fdinfo.int_of_fd file.f_descriptor)
;;


let open_report_mail common =
  Report.report#mail
    {
      m_common = common;
      m_filestate =
	match common.c_written with
	| false -> File_Opened
	| true -> File_Created
    }
;;


(* open_report_sql returns an Sql Obj option *)
let open_report_sql common =
  Report.report#sql
    {
      sr_common = common;
      sr_type = SQL_File_Opened;
    }
;;


let open_report_notify file written =
  let file_t =
    match written with
    | false -> File_Opened
    | true -> File_Created
  in
  Report.report#notify (New_notif (file, file_t))
;;


let open_report wd written (file, filesize) =

  if InotifyCaller.core#get_debug_event then
    print_file file;

  let has_what =
    match written with
    | false -> " has opened: "
    | true  -> " has created: "
  in

  let opening_date = new Date.date in

  print_endline
    (opening_date#get_str_locale^" - "^file.f_unix_login^has_what^file.f_name);

  Log.log (file.f_unix_login^has_what^file.f_name, Normal);

  let common = {
    c_file = file ;
    c_filesize =
      begin match written with
      | true -> None (* The filesize must be overriden as unknown (thus None) if the file is being written *)
      | false -> Some filesize
      end;
    c_first_known_offset = None ;
    c_last_known_offset = None ;
    c_opening_date = opening_date ;
    c_closing_date = None ;
    c_written = written ;
  } in

  open_report_notify file written;
  open_report_mail common;
  let sql_obj_opt = open_report_sql common in

  (* in_progress to add into Hashtbl Files_progress *)
  {
    ip_common = common ;
    ip_filesize_checked_again = false ;
    ip_offset_retrieval_errors = ref 0 ;
    ip_sql_connection = sql_obj_opt ;
  }
;;


let file_opened ?(written=false) wd name =
    match InotifyCaller.core#get_value wd with
      | None ->
	let err =
	  sprintf "%s was opened but its wd could not be found\n" name
	in
	Log.log (err, Error)

      | Some father ->

	if InotifyCaller.core#get_debug_event then
	  Printf.printf "[II] Folder: %s\n" father.path;

	let files = Files.get father.path name in

	Mutex.lock Files_progress.mutex_ht ;

	List.iter (fun (file, filesize) ->
	  match Hashtbl.mem Files_progress.ht (wd, file) with
	  | true -> ()
	  | false ->
	    let in_progress =
	      open_report wd written (file, filesize)
	    in
	    Hashtbl.add Files_progress.ht
	      (wd, file)
	      in_progress
	) files ;

	Mutex.unlock Files_progress.mutex_ht
(* eo Open, false *)
;;


let file_created wd name =
  file_opened ~written:true wd name
;;



(* Return the list of the files which stopped being accessed *)
let create_stop_files_list () =
  Hashtbl.fold (
    fun (wd2, f_file) values l_stop' ->

      (* Return new infos on a specific fdnum.
       * If the process is already closed, a Sys_error is triggered
       * by Fdinfo.get_fds
       *)
      let fdinprogress =
	try
	  Some (List.find (fun (fd, fdval) ->
	    fd = f_file.f_descriptor
	  ) (Fdinfo.get_fds f_file.f_program_pid))
	with _ -> None
      in

      match fdinprogress with
      | None -> ((wd2, f_file), ref values) :: l_stop'
      | Some (_, fdval) ->

	try
	  (* if it's a real path then true
	   * otherwise it's something like pipe:[160367] *)
	  match Sys.file_exists fdval with
	  | false -> ((wd2, f_file), ref values) :: l_stop'
	  | true -> l_stop'
	with
	| _ -> ((wd2, f_file), ref values) :: l_stop'
  ) Files_progress.ht []
;;

let override_filesize in_progress f_file written =
  match written with
  | false -> !in_progress.ip_common.c_filesize
  | true ->
    try
      let size =
	(Unix.stat (f_file.f_path^f_file.f_name)).st_size
      in
      Some (Int64.of_int size)
    with Unix_error (err_code, funct_name, fullpath) ->
      Log.log
	("In "^funct_name^" about "^fullpath^": "^(error_message err_code), Error);
      None
;;

let override_last_offset nfilesize in_progress written =

  match !in_progress.ip_common.c_last_known_offset with
  | None -> None (* unlikely to happen as data are read during the file_open event *)
  | Some last_offset' ->
    match nfilesize with
    | None ->
      (* best answer we have *)
      !in_progress.ip_common.c_last_known_offset

    | Some nfilesize' ->

      (* fix the offset to be equal to filesize
       * when creating the file *)
      if nfilesize' > last_offset' && written then
	nfilesize

      (* Sometimes, it happens that the offset is bigger than the filesize *)
      else if nfilesize' < last_offset' && not written then
	nfilesize

      else !in_progress.ip_common.c_last_known_offset
;;

let file_closed ?(written=false) wd name =

  if InotifyCaller.core#get_debug_event then begin
    match InotifyCaller.core#get_value wd with
    | None ->
      let err =
	sprintf "%s has been closed (nowrite) \
		 but I can't report it because I cannot find \
		 its wd info" name
      in
      Log.log (err, Error)

    | Some folder ->
      let path_quoted = Filename.quote folder.path in
      Printf.printf "[II] Folder: %s\n" path_quoted ;
  end;

  Mutex.lock Files_progress.mutex_ht ;
  let l_stop = create_stop_files_list () in
  Mutex.unlock Files_progress.mutex_ht ;

  List.iter (
    fun ((wd2, f_file), in_progress) ->

      Mutex.lock Files_progress.mutex_ht ;
      Hashtbl.remove Files_progress.ht (wd2, f_file);
      Mutex.unlock Files_progress.mutex_ht ;

      let closing_date = new Date.date in

      Printf.printf "%s - %s closed: %s (%d)\n"
	closing_date#get_str_locale
	f_file.f_unix_login
	f_file.f_name
	(Fdinfo.int_of_fd f_file.f_descriptor);

      Log.log (f_file.f_unix_login^" closed: "^f_file.f_name, Normal) ;

      (* update filesize in database if written
       * as filesize equaled None if written == true *)
      let nfilesize = override_filesize in_progress f_file written in

      (* update last_known_offset according to filesize and written *)
      (* if file could not be read or does not exist anymore then filesize = None *)
      let overriden_last_offset_opt = override_last_offset nfilesize in_progress written in

      in_progress :=
 	{ !in_progress with
	  ip_common = {
	    !in_progress.ip_common with
	      c_filesize = nfilesize ;
	      c_last_known_offset = overriden_last_offset_opt ;
	      c_closing_date = Some closing_date ;
	  }
	};

      (* *** SQL *** *)
      let sql_report = {
	sr_common = !in_progress.ip_common;
	sr_type = SQL_File_Closed ;
      } in
      ignore (Report.report#sql
		~sql_obj_opt:!in_progress.ip_sql_connection
		sql_report);
      (******************)


      (* *** Emails *** *)
      let tobemailed =
	{
	  m_common = !in_progress.ip_common;
	  m_filestate = File_Closed;
	}
      in
      Report.report#mail tobemailed;
      (******************)


      (* *** Notifications *** *)
      Report.report#notify (New_notif (f_file, File_Closed))
    (******************)
  ) l_stop
(* eo file_closed, false *)
;;
