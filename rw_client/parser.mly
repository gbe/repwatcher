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
%}
  

%token EQUAL
%token PVIRGULE
%token CLIENT_CA_PATH
%token CLIENT_CERT_PATH
%token CLIENT_KEY_PATH
%token CLIENT_KEY_PWD
%token <string>TXT
%token EOF

  
/* Point d'entrée de la grammaire */
%start configuration
  
/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Types_conf.configuration> configuration
  
%%


configuration: c_certs EOF {
   {
      c_certs = $1;
   }
}
;

c_certs:
| CLIENT_CA_PATH EQUAL txt
  CLIENT_CERT_PATH EQUAL txt
  CLIENT_KEY_PATH EQUAL txt
  client_key_pwd
  {{
    c_ca_path = $3;
    c_client_cert_path = $6;
    c_client_key_path = $9;
    c_client_key_pwd = $10;
  }}
;

client_key_pwd:
| { None }
| CLIENT_KEY_PWD EQUAL txt { Some $3 }
;



txt:
| TXT { if String.length $1 > 0 then $1 else raise Parse_error}
;
