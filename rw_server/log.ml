open Syslog
open Types
open Types_conf

let log (txt, log_level) =

  let conf = Config.get () in

  try
    let cid =
      openlog ~facility:`LOG_USER ~flags:[] "rw_server"
    in

(*    let log_it () =
      match log_level with
	| Normal -> syslog cid `LOG_NOTICE txt
	| Normal_Extra -> syslog cid `LOG_NOTICE txt
	| Error -> syslog cid `LOG_ERR txt
    in
*)
    begin
      match conf.c_log with
	| Disabled -> () (* don't log *)
	| Regular  ->
	  begin
	    match log_level with
	      | Normal       -> syslog cid `LOG_INFO txt
	      | Normal_Extra -> () (* don't log *)
	      | Error        ->
		prerr_endline txt;
		syslog cid `LOG_ERR txt
	  end
	| Debug -> syslog cid `LOG_DEBUG txt
    end;

    closelog cid;

  with _ -> prerr_endline "Syslog error"
;;
