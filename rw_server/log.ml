open Syslog
open Types
open Types_conf

let log (txt, log_level) =

  let conf = Config.get () in

  try
    let cid =
      openlog ~facility:`LOG_USER ~flags:[] "rw_server"
    in

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
	| Debug ->
	  begin
	    match log_level with
	      | (Normal | Normal_Extra) -> syslog cid `LOG_DEBUG txt
	      | Error ->
		prerr_endline txt;
		syslog cid `LOG_ERR txt
	  end;
    end;
    closelog cid;

  with _ -> prerr_endline "Syslog error"
;;
