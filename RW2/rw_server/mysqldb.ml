open Ast
open Ast_conf
open Mysql
open Unix

let cid = ref None;;

let connect c_sql =
  cid := Some (Mysql.connect c_sql)
;;

let query q =
  print_string q;
  
  match !cid with
    | None -> failwith "No cid"
    | Some cid -> exec cid q
;;

let fetch q =
  Mysql.fetch (query q)
;;
