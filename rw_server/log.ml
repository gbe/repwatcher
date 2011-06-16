open Unix
open Types
open Types_conf

(* if = 1 then logging should be disabled *)
let enoent = ref 0 ;;


(* Disable logging *)
let disable () =
  let conf = Config.get () in

  Config.conf :=
    Some {
      conf with c_log = {
	conf.c_log with l_verbosity = Disabled
      }
    }
;;


let log (txt, log_level) =

  let conf = Config.get() in
  let to_log = Printf.sprintf "%s\t%s\n" (Date.date()) txt in


  let rec open_fd ?(l=[O_WRONLY ; O_APPEND]) pathlog = 
    try
      let fd = Unix.openfile pathlog l 0o666 in
      
      (* if true then it means the exception was triggered before *)
      if List.mem O_CREAT l then begin
	Unix.fchmod fd 0o666 ;
	decr enoent
      end;
	
      Some fd
	
      
    with Unix_error (err,_,_) ->
      
      match err with
      | ENOENT -> (* no such file or directory *)

	begin match !enoent with
	  | 0 ->
	    incr enoent ;
	    open_fd ~l:[ O_WRONLY ; O_APPEND ; O_CREAT ] pathlog
	      
	  | 1 ->
	    disable () ;
	    None

	  | _ -> assert false
	end

      | _ ->
  
	disable () ;
	  
	let error = Printf.sprintf "Oops. Couldn't log due to this Unix error: \
	      %s. Logging feature disabled" (Unix.error_message err)
	in
	prerr_endline error;
	    
	(* fd = None because it failed to open *)
	None
  in


  let log_it ()  =

    (* Filename.concat automatically adds a "/" if necessary *)
    let pathlog =
      match log_level with
	| (Normal | Normal_Extra) ->
	  Filename.concat conf.c_log.l_directory "rw.log"

	| Error ->
	  Filename.concat conf.c_log.l_directory "rw.log.err"
    in

    match open_fd pathlog with
      | None -> prerr_endline ("An error occured, the file to log could neither \
	      be opened nor created. Is "^pathlog^" correct ?\n\
              Logging is now disabled.")
      | Some fd ->
	try
	  ignore (Unix.write fd to_log 0 (String.length to_log));
	  Unix.close fd
	    
	with Unix_error (err,_,_) ->
	  prerr_endline ("An error occured either trying to log in the file \
		or to close it: "^(Unix.error_message err))
  in

  begin
    (* Only the Errors get printed and on stderr *)
    match log_level with
      | Normal -> ()
      | Normal_Extra -> ()
      | Error -> prerr_endline to_log
  end;


  (* Depending on the user choice, we log *)
  match conf.c_log.l_verbosity with
    | Disabled -> () (* don't log *)
    | Regular  ->
      begin
	match log_level with
	  | Normal       -> log_it ()
	  | Normal_Extra -> () (* don't log *)
	  | Error        -> log_it ()
      end
    | Debug -> log_it ()
;;
