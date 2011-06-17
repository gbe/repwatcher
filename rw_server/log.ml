open Unix
open Types
open Types_conf

(* if = 1 then logging should be disabled *)
let enoent = ref 0 ;;

(*
  What I consider the root space is the time
  when the program starts running as root and the
  identity hasn't been dropped yet. If it starts running
  as regular user, it's still the root_time. It becomes
  user_time as soon as the root identity is dropped.
  Meanwhile, every logs are recorded in a FIFO.
  It's only in user_time that logs are really written
  into a file. This is a way to prevent root to create
  a file with his rights and in a place where the user
  isn't supposed to write. There is a race-competition
  between main process and remote process which could
  have caused this behavior.
*)
let root_time = ref true ;;
let fifo = Queue.create () ;;


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
      let fd = Unix.openfile pathlog l 0o644 in
      
      (* if true then it means the exception was triggered before *)
      if List.mem O_CREAT l then begin
	Unix.fchmod fd 0o644 ;
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
    match !root_time with
      | true -> Queue.add (txt, log_level) fifo
      | false ->

	(* Filename.concat automatically adds a "/" if necessary *)
	let pathlog =
	  match log_level with
	    | (Normal | Normal_Extra) ->
	      Filename.concat conf.c_log.l_directory "rw.log"
		
	    | Error ->
	      Filename.concat conf.c_log.l_directory "rw.log.err"
	in

	match open_fd pathlog with
	  | None ->
	    prerr_endline ("An error occured, the file to log could neither \
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

(* go to user time *)
let start_really_logging () =
  root_time := false ;

  match Queue.is_empty fifo with
    | true -> ()
    | false ->      
      Queue.iter log fifo ;
      Queue.clear fifo
;;
