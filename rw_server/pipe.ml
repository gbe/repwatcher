open Types
open Types_conf

exception Pipe_error

class pipe =
object(self)

  val mutable toread = None
  val mutable towrite = None

  (* Creation must be performed after the configuration file parsing has been done *)
  method create =
    let conf = (Config.cfg)#get in

    match conf.c_notify.n_remotely with
      | false -> ()
      | true ->
	try
	  let (tor,tow) = Unix.pipe () in
	  toread <- Some tor;
	  towrite <- Some tow
	with _ ->
	  Log.log ("A problem occured when creating the pipe", Error);
	  raise Pipe_error

  method get_toread =
    match toread with
      | None ->
	Log.log ("The toread pipe does not exist", Error);
	raise Pipe_error

      | Some toread' -> toread'

  method get_towrite =
    match towrite with
      | None ->
	Log.log ("The towrite pipe does not exist", Error);
	raise Pipe_error

      | Some towrite' -> towrite'

end ;;


(* This pipe is used when the father process communicates with its child *)
let father2child = new pipe ;;

(* This pipe is used when the child process communicates with its father *)
let child2father = new pipe ;;
