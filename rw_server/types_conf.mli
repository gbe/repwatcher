type watch = {
    w_directories        : string list   ;
    w_ignore_directories : string list   ; 
    w_ignore_users       : string list   ;
}

type mode_t =  Specified_programs | Unwanted_programs
type mode = mode_t * string list


type sql_db = Mysql.db = {
    dbhost : string option ;
    dbname : string option ;
    dbport : int option    ;
    dbpwd  : string option ;
    dbuser : string option ;
}

type certs_t = {
  c_ca_path : string;
  c_serv_cert_path : string;
  c_serv_key_path : string;
  c_serv_key_pwd : string option;
}

type server_t = {
    s_certs : certs_t option;
    s_port : int option;
    s_process_identity : string option;
    s_chroot : string option;
}

type notify = {
    n_locally  : bool;
    n_remotely : bool;
    n_parent_folders : int option;
}

type log_verbosity =
  | Disabled
  | Regular
  | Debug

type log = {
  l_verbosity : log_verbosity ;
  l_directory : string ;
}

type configuration = {
    c_watch : watch;
    c_mode : mode;
    c_process_identity : string option;
    c_mysql : sql_db;
    c_notify : notify;
    c_server : server_t option;
    c_log : log;
}
