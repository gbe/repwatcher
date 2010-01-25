(* TO DO

- Ajouter le SQL
- Modifier le fichier de config pour prendre en compte de nouveaux paramètres
- Voir pour mettre en surveillance des fichiers et pas seulement des dossiers (cf fichier de config donc)
- Revoir tout ce qui concerne le init et reinit

*)



open Unix
open Inotify
open Printf

open Ast
open Ast_conf

(* Attention en double avec core *)
let ht_iwatched = Hashtbl.create 4001
let fd = Inotify.init()
let (tor,tow) = Unix.pipe()

(* Attention en double avec core *)
let conf = ref {
  c_directories = [] ;
  c_mode = Unwanted_programs ;
  c_specified_programs = [] ; 
  c_unwanted_programs = [] ;
  c_sql = {
    dbhost = None ;
    dbname = None ;
    dbport = None ;
    dbpwd  = None ;
    dbuser = None ;
  }
}



let date () =
  let date = (localtime(time())) in

  (* Starts in 1900 *)
  let year = string_of_int (1900 + date.tm_year) in
    (* month = 0..11 *)
  let month = string_of_int (1 + date.tm_mon) in
  let day = string_of_int date.tm_mday in
  let hour = string_of_int date.tm_hour in
  let minute = string_of_int date.tm_min in
  let second = string_of_int date.tm_sec in
  
  (* 2008-01-19 16:21:00 *)
  sprintf "%s-%s-%s %s:%s:%s" year month day hour minute second
;;


let log txt =
  let to_log = Printf.sprintf "%s\t%s" (date()) txt in
  ignore (Unix.system ("echo \""^to_log^"\" >> log.txt"))
;;



let notify txt =
  printf "Notify: %s\n" txt;  
  (* Send in the pipe for the server to send to the clients *)
  ignore (Unix.write tow txt 0 (String.length txt))
;;


(* Fonction main *)
let _ =
  
  (* Verifie que le programme est lance en root seulement *)
(*  if (Unix.getuid()) <> 0 then
    begin
      print_endline "This program has to be executed as root\n";
      exit 1
    end;
*)


  (* Load the configuration file and then watch the directories given in it *)
  (* init (); *)


  match Unix.fork() with
    | 0 -> Ssl_server.run tor
    | _ ->
	begin
	  
	  let print_ht () =
	    Hashtbl.iter (fun key value -> 
			    printf "\n--------------\n'%s'(%d) est le père de :\n" value.path (int_of_wd key);
			    List.iter (fun child -> printf "%d\t" (int_of_wd child)) value.wd_children
			 ) ht_iwatched 
	  in
	    
	    print_ht ();
	    
	    Pervasives.flush Pervasives.stdout;
	    
	    notify "Repwatcher is watching youuu ! :)";
	    log "Repwatcher is watching youuu ! :)";	    
     
	      while true do
		
		let _,_,_ = Unix.select [ fd ] [] [] (-1.) in
		let event_l = Inotify.read fd in
		  
		  List.iter (fun event ->
			       
			       (* let (_,tel,_,_) = event in
				  
				  print_endline ("\n-------");
				  List.iter (fun type_event -> print_endline (string_of_event type_event) ) tel;
				  print_endline ("---------\n");
			       *)
			       
			       let _ = Core.what_to_do event conf fd in
		
				 printf "Hashtable :%d\n" (Hashtbl.length ht_iwatched);
				 Pervasives.flush Pervasives.stdout
			    ) event_l;
	      done;
	      
	      Unix.close fd
	end