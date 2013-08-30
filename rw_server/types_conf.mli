type watch = {
    w_directories        : string list   ;
    w_ignore_directories : string list   ; 
    w_ignore_users       : string list   ;
}

type mode_t =  Specified_programs | Unwanted_programs
type mode = mode_t * string list


type sql_param = {
    sql_rdbms  : string ;
    sql_dbhost : string ;
    sql_dbname : string ;
    sql_dbport : int option ;
    sql_dbpwd  : string ;
    sql_dbuser : string ;
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

type smtp_credentials = {
  cred_username : string;
  cred_passwd : string  
}

type smtp = {
  sm_host : string;
  sm_port : int;
  sm_credentials : smtp_credentials option;
  sm_ssl : bool
}

type email = {
    e_open : bool;
    e_close : bool;
    e_sender_name : string;
    e_sender_address : string;
    e_recipients : string list;
    e_smtp : smtp;
}

type configuration = {
    c_watch : watch;
    c_mode : mode;
    c_process_identity : string option;
    c_sql : sql_param option;
    c_notify : notify;
    c_server : server_t option;
    c_email : email option;
    c_log : log_verbosity;
}
