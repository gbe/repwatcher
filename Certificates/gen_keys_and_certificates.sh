#!/bin/bash

#set -x
#set -e


function underline {

	for (( i = 0 ; i < ${#1} ; i++ )) do
		echo -n "-"
	done;
	echo -e "\n"
}

function create_ca_key {
	clear &&
	echo    "1/8 Create the CA key" &&
	echo -e "---------------------\n" &&
	openssl genrsa -aes256 1024 > CA/CA.key
}

function create_ca_cert {
	clear &&	
	echo    "2/8 Create the CA cert from the CA key" &&
	echo -e "--------------------------------------\n" &&
	echo "How many days should this certificate be valid ?" &&
	echo -n "Number of days: " &&
	read cert_days &&
	openssl req -new -x509 -days $cert_days -key CA/CA.key > CA/CA.crt
}

function create_key {
	clear &&

	TXT="$2 Create the key $1"
	echo $TXT;
	underline "$TXT"

	openssl genrsa 1024 > $1 &&
	echo -e "==> Key $1 created\n\n"
}

# arg1 = key (in)
# arg2 = cert.csr (out)
# arg3 = 2/8 (feedback)
function request_signed_cert {
	# No clear here so as to see that the key
	# has been created for the server/client
	TXT="$3 Fill in the informations to request a signed certificate for $2"
	echo $TXT &&
	underline "$TXT" &&

	openssl req -new -key $1 > $2
}

function sign_cert {
	clear &&
	TXT="$2 Sign the request certificate $1" &&
	echo $TXT;
	underline "$TXT";

	certsigned_out=${1:0:$((${#1}-4))}
	openssl x509 -req -in $1 -out $certsigned_out.crt -CA CA/CA.crt\
	-CAkey CA/CA.key -CAcreateserial -CAserial CA.srl
}

function failure {
	echo -e "\n==> Failure. $1\n" &&
	exit 1
}


function exit_or_continue {
	echo -n -e "\nThe folder $1 already exists. "
	echo "If you continue, it will be overwritten.";
	echo -n "Would you like to exit now (Y/n)? ";
	read ans;

	case $ans in
		[yY])
			echo "Exiting..." ;
			exit 1 ;;
		[nN])
			echo "Continuing..." ;;
		*)
			echo "Exiting..." ;
			exit 1
	esac
}

if [ ! -d "CA" ]; then
	mkdir CA
else
	exit_or_continue "CA" && rm -f CA/*
fi

if [ ! -d "server" ]; then
	mkdir server
else
	exit_or_continue "server" && rm -f server/*
fi

if [ ! -d "client" ]; then
	mkdir client
else
	exit_or_continue "client" && rm -f client/*
fi



###############################################
# CA
###############################################
create_ca_key || failure "CA key not created";
create_ca_cert || failure "CA cert not created";
###############################################



###############################################
# Server
###############################################
create_key server/rw_serv.key 3/8 || failure "Could not create the server's key";

request_signed_cert server/rw_serv.key server/rw_serv.csr 4/8 || failure "Couldn't request a signed certificate";

sign_cert server/rw_serv.csr 5/8 && rm -f CA.srl server/rw_serv.csr || failure "CA couldn't sign server certificate";

echo -e "\n==> Server certificate ready."
###############################################



###############################################
# Client
###############################################
create_key client/rw_client.key 6/8 || failure "Could not create the client's key";

request_signed_cert client/rw_client.key client/rw_client.csr 7/8 || failure "Couldn't request a signed certificate";

sign_cert client/rw_client.csr 8/8 && rm -f CA.srl client/rw_client.csr || failure "CA couldn't sign client certificate";

echo -e "\n==> Client certificate ready.\n";
###############################################


echo "Results :";
echo "---------";

echo "CA's key and certificate created with success";
echo "Server and Client's key and certificate created and signed by the CA with success";

exit 0;