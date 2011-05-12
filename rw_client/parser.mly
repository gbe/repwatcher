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
