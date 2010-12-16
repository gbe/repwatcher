/*
    Repwatcher
    Copyright (C) 2009-2010  Gregory Bellier

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
   open Ast_conf 
%}
  

%token EQUAL
%token PVIRGULE
%token DIRECTORIES
%token IGNORE_DIRECTORIES
%token IGNORE_USERS
%token SPECIFIED_PROGRAMS
%token UNWANTED_PROGRAMS
%token SQL_LOGIN
%token SQL_PSWD
%token SQL_HOST
%token SQL_PORT
%token SQL_DBNAME
%token NOTIFY_LOCALLY
%token NOTIFY_REMOTELY
%token LOG_LEVEL
%token MAIN_IDENTITY_FALLBACK
%token <string>TXT
%token EOF

  
/* Point d'entrée de la grammaire */
%start conf
  
/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast_conf.configuration> conf
  
%%


conf: watch mode mysql notify log main_identity_fallback EOF {
   {
      c_watch = $1;
      c_mode = $2;
      c_mysql = $3;
      c_notify = $4;
      c_log = $5;
      c_main_proc_id_fallback = $6;
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
| DIRECTORIES EQUAL txt_plus_list IGNORE_DIRECTORIES EQUAL txt_star_list
      {{
       w_directories = $3;
       w_ignore_directories = $6;
       w_ignore_users = [];      	
      }}	   	   
| DIRECTORIES EQUAL txt_plus_list IGNORE_USERS EQUAL txt_star_list
      {{
       w_directories = $3;
       w_ignore_directories = [];
       w_ignore_users = $6;      	
      }}
| DIRECTORIES EQUAL txt_plus_list IGNORE_DIRECTORIES EQUAL txt_star_list IGNORE_USERS EQUAL txt_star_list
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


mysql:
| SQL_LOGIN EQUAL txt_plus
  SQL_PSWD EQUAL txt_plus
  SQL_HOST EQUAL txt_plus
  SQL_PORT EQUAL int_option
  SQL_DBNAME EQUAL txt_plus
      {{
       dbhost = Some $9;
       dbname = Some $15;
       dbport = $12;
       dbpwd  = Some $6;
       dbuser = Some $3;
      }}

| SQL_LOGIN EQUAL txt_plus
  SQL_PSWD EQUAL txt_plus
  SQL_HOST EQUAL txt_plus
  SQL_DBNAME EQUAL txt_plus
      {{
       dbhost = Some $9;
       dbname = Some $12;
       dbport = None;
       dbpwd  = Some $6;
       dbuser = Some $3;
      }}
;

notify:
|  NOTIFY_LOCALLY EQUAL true_or_false
   NOTIFY_REMOTELY EQUAL true_or_false
      {{
       n_locally = $3;
       n_remotely = $6;
      }}

|  NOTIFY_REMOTELY EQUAL true_or_false
   NOTIFY_LOCALLY EQUAL true_or_false
      {{
       n_locally = $6;
       n_remotely = $3;
      }}   
;


log:
/* If the log part is commented then it means logging is disabled */
| { Disabled }
| LOG_LEVEL EQUAL int_option {
   match $3 with
   | None -> Regular
   | Some level ->
      match level with
      | 0 -> Disabled
      | 1 -> Regular
      | 2 -> Debug
      | _ -> raise Parse_error
   }
;

main_identity_fallback:
| { None }
| MAIN_IDENTITY_FALLBACK EQUAL txt_plus { Some $3 }
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

int_option:
| txt_star {
  if (String.length $1) = 0 then
    None
  else 
    try
      Some (int_of_string $1)
    with
      Failure _ -> raise Parse_error         
}
;
