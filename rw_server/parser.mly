/*
    Repwatcher
    Copyright (C) 2009-2011  Gregory Bellier

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/



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
%token MYSQL_LOGIN
%token MYSQL_PSWD
%token MYSQL_HOST
%token MYSQL_PORT
%token MYSQL_DBNAME
%token NOTIFY_LOCALLY
%token NOTIFY_REMOTELY
%token REMOTE_CA_PATH
%token REMOTE_SERV_CERT_PATH
%token REMOTE_SERV_KEY_PATH
%token REMOTE_SERV_KEY_PWD
%token REMOTE_IDENTITY_FALLBACK
%token REMOTE_CHROOT
%token PARENT_FOLDERS
%token LOG_LEVEL
%token MAIN_IDENTITY_FALLBACK
%token <string>TXT
%token EOF

  
/* Point d'entrée de la grammaire */
%start configuration
  
/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Types_conf.configuration> configuration
  
%%


configuration: watch mode main_identity_fallback mysql notify server log EOF {
   {
      c_watch = $1;
      c_mode = $2;
      c_main_proc_id_fallback = $3;
      c_mysql = $4;
      c_notify = $5;
      c_server = $6;
      c_log = $7;
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

main_identity_fallback:
| { None }
| MAIN_IDENTITY_FALLBACK EQUAL txt_plus { Some $3 }
;

mysql:
| MYSQL_LOGIN EQUAL txt_plus
  MYSQL_PSWD EQUAL txt_plus
  MYSQL_HOST EQUAL txt_plus
  MYSQL_PORT EQUAL int
  MYSQL_DBNAME EQUAL txt_plus
      {{
       dbhost = Some $9;
       dbname = Some $15;
       dbport = Some $12;
       dbpwd  = Some $6;
       dbuser = Some $3;
      }}

| MYSQL_LOGIN EQUAL txt_plus
  MYSQL_PSWD EQUAL txt_plus
  MYSQL_HOST EQUAL txt_plus
  MYSQL_DBNAME EQUAL txt_plus
      {{
       dbhost = Some $9;
       dbname = Some $12;
       dbport = None;
       dbpwd  = Some $6;
       dbuser = Some $3;
      }}
;

notify:
|   NOTIFY_LOCALLY EQUAL true_or_false
    NOTIFY_REMOTELY EQUAL true_or_false
    PARENT_FOLDERS EQUAL txt_plus
    {
     try
       let nb =
	 
	 let i = int_of_string $9 in
	 if i < 0 then
	   raise Parse_error
	 else if i = 0 then
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
| { None }

| certs
    {
     check_options $1 ;
     Some ({
	   s_certs = $1;
	   s_process_identity = None;
	   s_chroot = None;
	 })
   }
    
| certs REMOTE_IDENTITY_FALLBACK EQUAL txt_plus
    {
     check_options $1 ;
     Some ({
	   s_certs = $1;
	   s_process_identity = Some $4;
	   s_chroot = None;
	 })
   }
    
| certs REMOTE_CHROOT EQUAL txt_plus
    {
     check_options $1 ;
     Some ({
	   s_certs = $1;
	   s_process_identity = None;
	   s_chroot = Some $4;
	 })
   }
    
| certs REMOTE_IDENTITY_FALLBACK EQUAL txt_plus
       REMOTE_CHROOT EQUAL txt_plus
    {
     check_options $1 ;
     Some ({
	   s_certs = $1;
	   s_process_identity = Some $4;
	   s_chroot = Some $7;
	 })
   }
;



certs:
| REMOTE_CA_PATH EQUAL txt_plus
  REMOTE_SERV_CERT_PATH EQUAL txt_plus
  REMOTE_SERV_KEY_PATH EQUAL txt_plus
  REMOTE_SERV_KEY_PWD EQUAL txt_plus
  {
    Some
     {
      c_ca_path = $3;
      c_serv_cert_path = $6;
      c_serv_key_path = $9;
      c_serv_key_pwd = $12;
    }
 }
| { None }
;

log:
/* If the log part is commented then it means logging is disabled */
| { Disabled }
| LOG_LEVEL EQUAL int {
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

int:
| txt_plus {
  try
    int_of_string $1
  with Failure _ -> raise Parse_error
}
;
