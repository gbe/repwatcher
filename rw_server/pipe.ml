(* This pipe is used when the father process communicates with its child *)
let (tor,tow)   = Unix.pipe() ;;

(* This pipe is used when the child process communicates with its father *)
let (tor2,tow2) = Unix.pipe() ;;
