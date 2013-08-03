open Syslog
open Types
open Types_conf

exception Cid_error

class syslog =
object(self)
  
(* use Regular as a default value to not have a circulary dependency log.ml <-> config.ml
 * Once the config file has been read, the value must be updated
 *)
  val mutable c_log = Regular
  val mutable syslog_cid = None

  method private _get_cid=
    match syslog_cid with
      | None -> raise Cid_error
      | Some cid' -> cid'

  method openlog_connect =
    try
      syslog_cid <-
	Some (openlog ~facility:`LOG_USER ~flags:[] "rw_server");
    with _ -> prerr_endline "Syslog error: openlog"

  method private _log syslog_level txt =
    try
      syslog self#_get_cid syslog_level txt
    with 
      | Cid_error -> prerr_endline "Syslog error: sendlog cid error"
      | _ -> prerr_endline "Syslog error: sendlog"


  method sendlog txt log_level =
    match c_log with
      | Disabled -> () (* don't log *)
      | Regular  ->
	begin
	  match log_level with
	    | Normal       -> self#_log `LOG_INFO txt
	    | Normal_Extra -> () (* don't log *)
	    | Error        ->
	      prerr_endline txt;
	      self#_log `LOG_ERR txt
	end
      | Debug ->
	begin
	  match log_level with
	    | (Normal | Normal_Extra) -> self#_log `LOG_DEBUG txt
	    | Error ->
	      prerr_endline txt;
	      self#_log `LOG_ERR txt
	end

  method closelog =
    try
      closelog self#_get_cid
    with
      | Cid_error -> prerr_endline "Syslog error: closelog cid error"
      | _ -> prerr_endline "Syslog error: closelog"

  method set_config c_log' =
    c_log <- c_log'

end;;

let sysl = new syslog ;;

let log (txt, log_level) =
  sysl#openlog_connect;
  sysl#sendlog txt log_level;
  sysl#closelog
;;
