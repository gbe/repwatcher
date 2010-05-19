/*
    This file is part of Repwatcher.

    Repwatcher is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Repwatcher is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Repwatcher; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/





%{
  open Ast_conf
%}
  
%token DQUOTE
%token EQUAL
%token PVIRGULE
%token IGNORE_USERS
%token DIRECTORIES
%token IGNORE_DIRECTORIES
%token MODE
%token SPEC
%token UNWANTED
%token SQL_LOGIN
%token SQL_PSWD
%token SQL_HOST
%token SQL_PORT
%token SQL_DBNAME
%token NOTIFY_LOCALLY
%token NOTIFY_REMOTELY
%token LOG_LEVEL
%token YES
%token NO
%token <string>TXT
%token <string>DIGITS
%token EOF 

  
/* Point d'entrée de la grammaire */
%start deb
  
/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast_conf.configuration> deb
  
%%


  deb:
 DIRECTORIES EQUAL txt_list
 IGNORE_DIRECTORIES EQUAL txt_digits_list_star
 IGNORE_USERS EQUAL txt_digits_list_star
 MODE EQUAL mode
 SPEC EQUAL txt_list
 UNWANTED EQUAL txt_digits_list_star
 SQL_LOGIN EQUAL txt_or_digits
 SQL_PSWD EQUAL txt_or_digits
 SQL_HOST EQUAL txt
 SQL_PORT EQUAL digits_int_option
 SQL_DBNAME EQUAL txt_or_digits
 NOTIFY_LOCALLY EQUAL yes_or_no
 NOTIFY_REMOTELY EQUAL yes_or_no
 LOG_LEVEL EQUAL digits_int_option
 EOF { 
      {
	c_directories = $3;
	c_ignore_directories = $6;
	c_ignore_users = $9;
	c_mode = $12;
	c_specified_programs = $15;
	c_unwanted_programs = $18;
	c_sql = {
	  dbhost = Some $27;
	  dbname = Some $33;
	  dbport = $30;
	  dbpwd  = Some $24;
	  dbuser = Some $21;
	};
       c_notify_loc = $36;
       c_notify_rem = $39;
       c_log_level  =
	  match $42 with
	    | None -> 1 (* 1 is the default log_level *)
	    | Some i ->
		match i with
		  | (0|1|2) -> i
		  | _       -> raise Parse_error
      }
    }
  ;


txt_list:
| txt { [$1] }
| txt_list PVIRGULE txt { $3::$1 }
;

txt:
| DQUOTE TXT DQUOTE { $2 }
;

digits:
| DQUOTE DIGITS DQUOTE { $2 }
;

digits_int_option:
| DQUOTE DQUOTE { None }
| DQUOTE DIGITS DQUOTE { Some (int_of_string $2) }
;

txt_or_digits:
| txt { $1 }
| digits { $1 }
;

 mode:
| DQUOTE SPEC DQUOTE { Specified_programs }
| DQUOTE UNWANTED DQUOTE { Unwanted_programs }
;

txt_digits_list_star:
| DQUOTE DQUOTE { [] }
| txt_digits_list { $1 }
;

txt_digits_list:
| txt_or_digits { [$1] }
| txt_digits_list PVIRGULE txt_or_digits { $3::$1 }
;

yes_or_no:
| DQUOTE YES DQUOTE { true  }
| DQUOTE NO DQUOTE  { false }
;
