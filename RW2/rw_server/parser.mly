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
   open Types_conf 
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
%token REMOTE_IDENTITY_FALLBACK
%token REMOTE_CHROOT
%token PARENT_FOLDERS
%token LOG_LEVEL
%token MAIN_IDENTITY_FALLBACK
%token <string>TXT
%token EOF

  
/* Point d'entrée de la grammaire */
%start conf
  
/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Types_conf.configuration> conf
  
%%


conf: watch mode main_identity_fallback mysql notify log EOF {
   {
      c_watch = $1;
      c_mode = $2;
      c_main_proc_id_fallback = $3;
      c_mysql = $4;
      c_notify = $5;
      c_log = $6;
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
|  NOTIFY_LOCALLY EQUAL true_or_false notif_remote PARENT_FOLDERS EQUAL txt_plus
    {
     try
       let nb = int_of_string $7 in
       begin if nb <= 0 then
	 raise Parse_error
       end;
       {
	n_locally = $3;
	n_remotely = $4;
	n_parent_folders = nb;
      }
     with Failure "int_of_string" -> raise Parse_error
   }
;

notif_remote:
|   NOTIFY_REMOTELY EQUAL true_or_false
      {{
       r_activate = $3;
       r_process_identity = None;
       r_chroot = None;
      }}
|   NOTIFY_REMOTELY EQUAL true_or_false
    REMOTE_IDENTITY_FALLBACK EQUAL txt_plus
      {{
       r_activate = $3;
       r_process_identity = Some $6;
       r_chroot = None;
      }}
|   NOTIFY_REMOTELY EQUAL true_or_false
    REMOTE_CHROOT EQUAL txt_plus
      {{
       r_activate = $3;
       r_process_identity = None;
       r_chroot = Some $6;
      }}
|   NOTIFY_REMOTELY EQUAL true_or_false
    REMOTE_IDENTITY_FALLBACK EQUAL txt_plus
    REMOTE_CHROOT EQUAL txt_plus
      {{
       r_activate = $3;
       r_process_identity = Some $6;
       r_chroot = Some $9;
      }}
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
