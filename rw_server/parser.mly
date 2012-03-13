%{
open Types_conf;;

let is_server_activated = ref false ;;

let check_options cert =
  match (!is_server_activated, cert) with
  | true, None -> raise Parse_error
  | _ -> ()
;;

%}
  

%token EQUAL
%token PVIRGULE
%token DIRECTORIES
%token IGNORE_DIRECTORIES
%token IGNORE_USERS
%token SPECIFIED_PROGRAMS
%token UNWANTED_PROGRAMS
%token PROCESS_IDENTITY
%token MYSQL_LOGIN
%token MYSQL_PSWD
%token MYSQL_HOST
%token MYSQL_PORT
%token MYSQL_DBNAME
%token NOTIFY_LOCALLY
%token NOTIFY_REMOTELY
%token SERVER_CA_PATH
%token SERVER_CERT_PATH
%token SERVER_KEY_PATH
%token SERVER_KEY_PWD
%token SERVER_PORT
%token SERVER_PROCESS_IDENTITY
%token SERVER_PROCESS_CHROOT
%token EMAIL_OPEN
%token EMAIL_CLOSE
%token EMAIL_SENDER_NAME
%token EMAIL_SENDER_ADDRESS
%token EMAIL_RECIPIENTS
%token PARENT_FOLDERS
%token LOG_LEVEL
%token <string>TXT
%token EOF


  
/* Point d'entrée de la grammaire */
%start configuration
  
/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Types_conf.configuration> configuration
  
%%


configuration: watch mode process_identity mysql notify server email log EOF {
   {
      c_watch = $1;
      c_mode = $2;
      c_process_identity = $3;
      c_mysql = $4;
      c_notify = $5;
      c_server = $6;
      c_email = $7;
      c_log = $8;
   }
}
;

watch:
| DIRECTORIES EQUAL txt_plus_list
      {{
       w_directories = $3;
       w_ignore_directories = [];
       w_ignore_users = [];
     }}
| DIRECTORIES EQUAL txt_plus_list IGNORE_DIRECTORIES EQUAL txt_plus_list
      {{
       w_directories = $3;
       w_ignore_directories = $6;
       w_ignore_users = [];      	
      }}	   	   
| DIRECTORIES EQUAL txt_plus_list IGNORE_USERS EQUAL txt_plus_list
      {{
       w_directories = $3;
       w_ignore_directories = [];
       w_ignore_users = $6;      	
      }}
| DIRECTORIES EQUAL txt_plus_list IGNORE_DIRECTORIES EQUAL txt_plus_list IGNORE_USERS EQUAL txt_plus_list
      {{
       w_directories = $3;
       w_ignore_directories = $6;
       w_ignore_users = $9;
      }}
;


mode:
| SPECIFIED_PROGRAMS EQUAL txt_plus_list { (Specified_programs, $3) }
| UNWANTED_PROGRAMS EQUAL txt_star_list { (Unwanted_programs, $3) }
;

process_identity:
| { None }
| PROCESS_IDENTITY EQUAL txt_plus { Some $3 }
;

mysql:
| MYSQL_LOGIN EQUAL txt_plus
  MYSQL_PSWD EQUAL txt_plus
  MYSQL_HOST EQUAL txt_plus
  MYSQL_PORT EQUAL uint
  MYSQL_DBNAME EQUAL txt_plus
      {
	Some {
	  dbhost = Some $9;
	  dbname = Some $15;
	  dbport = Some $12;
	  dbpwd  = Some $6;
	  dbuser = Some $3;
	}
      }

| MYSQL_LOGIN EQUAL txt_plus
  MYSQL_PSWD EQUAL txt_plus
  MYSQL_HOST EQUAL txt_plus
  MYSQL_DBNAME EQUAL txt_plus
      {
	Some {
	  dbhost = Some $9;
	  dbname = Some $12;
	  dbport = None;
	  dbpwd  = Some $6;
	  dbuser = Some $3;
	}
      }
;

notify:
|   NOTIFY_LOCALLY EQUAL true_or_false
    NOTIFY_REMOTELY EQUAL true_or_false
    PARENT_FOLDERS EQUAL uint
    {
     try
       let nb =
	 let i = $9 in
	 if i = 0 then
	   None
	 else
	   Some i
       in

       is_server_activated := $6 ;
       {
	n_locally = $3;
	n_remotely = $6;
	n_parent_folders = nb;
      }
     with Failure "int_of_string" -> raise Parse_error
}
|  NOTIFY_LOCALLY EQUAL true_or_false
   NOTIFY_REMOTELY EQUAL true_or_false
    {
     is_server_activated := $6 ;
     {
      n_locally = $3;
      n_remotely = $6;
      n_parent_folders = None;
    }
   }
;

server:
/*  If there is None here, there is a reduce/reduce conflict
    due to s_certs which can be also None.
    
    2 possible ways in case of a None :
    notify -> serv -> s_certs
    which could also be interpreted as
    notify -> s_certs
*/

/* server can keep its optional type by using the None from s_certs */
/* | { None } */
   | s_certs s_port s_process_identity s_chroot
    {
     check_options $1;
     Some
       {
	s_certs = $1;
	s_port = $2;
	s_process_identity = $3;
	s_chroot = $4;
      }
   }
;


s_certs:
| { None }
| SERVER_CA_PATH EQUAL txt_plus
  SERVER_CERT_PATH EQUAL txt_plus
  SERVER_KEY_PATH EQUAL txt_plus
  serv_key_pwd
  {
    Some
     {
      c_ca_path = $3;
      c_serv_cert_path = $6;
      c_serv_key_path = $9;
      c_serv_key_pwd = $10;
    }
 }
;

serv_key_pwd:
| { None }
| SERVER_KEY_PWD EQUAL txt_plus { Some $3 }
;

s_port:
| { None }
| SERVER_PORT EQUAL uint_pos { Some $3 }

s_process_identity:
| { None }
| SERVER_PROCESS_IDENTITY EQUAL txt_plus { Some $3 }
;

s_chroot:
| { None }
| SERVER_PROCESS_CHROOT EQUAL txt_plus { Some $3 }
;


email:
| EMAIL_OPEN EQUAL true_or_false
  EMAIL_CLOSE EQUAL true_or_false
    {
     match $3 || $6 with
     | true -> raise Parse_error
     | false ->
	 {
	  e_open = false;
	  e_close = false;
	  e_sender_name = "";
	  e_sender_address = "";
	  e_recipients = [];
	}
   }

| EMAIL_OPEN EQUAL true_or_false
  EMAIL_CLOSE EQUAL true_or_false
  EMAIL_SENDER_NAME EQUAL txt_plus
  EMAIL_SENDER_ADDRESS EQUAL txt_plus
  EMAIL_RECIPIENTS EQUAL txt_plus_list
    {
     match $3 || $6 with
     | false ->
	 {
	  e_open = false;
	  e_close = false;
	  e_sender_name = "";
	  e_sender_address = "";
	  e_recipients = [];
	}
     | true ->
	 if List.length $15 == 0 then
	   raise Parse_error
	 else
	   {
	    e_open = $3;
	    e_close = $6;
	    e_sender_name = $9;
	    e_sender_address = $12;
	    e_recipients = $15;
	  }
   }
;

log:
/* If the log part is commented then it means logging is disabled */
| { Disabled }
| LOG_LEVEL EQUAL uint {
  match $3 with
  | 0 -> Disabled
  | 1 -> Regular
  | 2 -> Debug
  | _ -> raise Parse_error
}
;

txt_plus_list:
| txt_plus { [$1] }
| txt_plus_list PVIRGULE txt_plus { $3::$1 }
;


txt_plus:
| TXT { if String.length $1 > 0 then $1 else raise Parse_error}
;

txt_star:
| TXT { $1 }
;

txt_star_list:
| txt_star { if (String.length $1) == 0 then [] else [$1] }
| txt_star_list PVIRGULE txt_star { $3::$1 }
;

true_or_false:
| txt_plus {
  match $1 with
  | ("Y" | "y") -> true
  | ("N" | "n") -> false
  | _ -> raise Parse_error
}
;

uint_pos:
| uint {
  if $1 = 0 then
    raise Parse_error
  else
    $1
}

uint:
| txt_plus {
  try
    let i = int_of_string $1 in

    if i < 0 then
      raise Parse_error
    else
      i
  with Failure _ -> raise Parse_error
}
;
