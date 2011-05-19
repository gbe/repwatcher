open Mysql
open Types
open Types_conf
open Unix

let ml2str = Mysql.ml2str ;;
let ml2int = Mysql.ml2int ;;

let connect () =
  try
    let cid = Mysql.connect (Config.get()).c_mysql in
    Log.log ("Connected to MySQL", Normal_Extra) ;
    Some cid

  with Mysql.Error error ->
    Log.log (error, Error);
    None
;;

let connect_without_db () =

  let m = (Config.get()).c_mysql in

  try
    let cid = Mysql.connect { m with dbname = None } in
    Log.log ("Connected to MySQL", Normal_Extra) ;
    Some cid

  with Mysql.Error error ->
    Log.log (error, Error);
    None
;;

let disconnect cid =
  try
    Mysql.disconnect cid;
    Log.log ("Disconnected from MySQL", Normal_Extra)

  with Mysql.Error error ->
    Log.log ("RW couldn't disconnect from Mysql: "^error, Error)
;;



let map res =
  try
    (* row = string option array, the option is if a field is NULL *)
    let rows = Mysql.map res (fun row -> row) in
    Log.log ("Query successfully mapped", Normal_Extra);

    rows

  with Mysql.Error error ->
    Log.log (error, Error);
    []
;;


let query cid q =
  
  Log.log (("Next SQL query to compute:\n"^q^"\n"), Normal_Extra) ;
  
  try
    let res = exec cid q in
    
    match status cid with
    | (StatusOK | StatusEmpty) ->
	Log.log ("Query successfully executed", Normal_Extra);
	Some res
    | StatusError _ ->
	begin match errmsg cid with
	| None ->
	    Log.log ("Oops. Mysqldb.query had an error and the SGBD \
		       can't tell which one", Error)
	| Some errmsg' -> Log.log (errmsg', Error)
	end;
	None
  with Mysql.Error error ->
    Log.log (error, Error) ;
    None
;;


let create_db dbname =

  let q =
    Printf.sprintf "CREATE DATABASE IF NOT EXISTS %s" dbname
  in

  match connect_without_db () with
    | None -> assert false
    | Some cid ->

      try
	let _ = exec cid q in

	begin
	  match status cid with
	    | StatusOK ->
	      Log.log (("Database "^dbname^" successfully created"), Normal_Extra)

	    | StatusEmpty -> ()

	    | StatusError _ ->
	      begin
		match errmsg cid with
		  | None ->
		    Log.log ("Oops. Mysqldb.create_db had an error and the SGBD \
			     doesn't know why", Error)

		  | Some errmsg' ->
		    Log.log (errmsg', Error)
	      end ;
	      exit 2
	end ;

	disconnect cid

      with
	  Mysql.Error error ->
	    Log.log (error, Error) ;
	    exit 2
;;

let create_table_accesses () =

  let q =
    Printf.sprintf "CREATE TABLE IF NOT EXISTS `accesses` (\
  `ID` int(10) NOT NULL AUTO_INCREMENT,\
  `LOGIN` varchar(20) NOT NULL,\
  `PROGRAM` varchar(26) NOT NULL,\
  `PROGRAM_PID` int(8) NOT NULL,\
  `PATH` varchar(512) NOT NULL,\
  `FILENAME` varchar(256) NOT NULL,\
  `FILESIZE` bigint(20) unsigned NOT NULL,\
  `FIRST_KNOWN_OFFSET` bigint(20) unsigned DEFAULT NULL,\
  `LAST_KNOWN_OFFSET` bigint(20) unsigned DEFAULT NULL,\
  `OPENING_DATE` datetime NOT NULL,\
  `CLOSING_DATE` datetime DEFAULT NULL,\
  `IN_PROGRESS` tinyint(1) unsigned NOT NULL,\
  PRIMARY KEY (`ID`)\
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;"
  in

  match connect () with
    | None -> assert false
    | Some cid ->

      try
	let _ = exec cid q in

	begin
	  match status cid with
	    | (StatusOK | StatusEmpty) ->
	      Log.log (("Table accesses successfully created"), Normal_Extra)	 

	    | StatusError _ ->
	      begin
		match errmsg cid with
		  | None ->
		    Log.log ("Oops. Mysqldb.create_table_accesses had an error and the SGBD \
			     doesn't know why", Error)

		  | Some errmsg' ->
		    Log.log (errmsg', Error)
	      end ;
	      exit 2
	end ;

	disconnect cid

      with
	  Mysql.Error error ->
	    Log.log (error, Error) ;
	    exit 2
;;
