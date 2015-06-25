#!/bin/bash

#set -x
#set -e

CA_PATH="CA"
SERVER_PATH="server"
CLIENT_PATH="client"

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
	openssl genrsa -aes256 4096 > $CA_PATH/CA.key
}

function create_ca_cert {
	clear &&	
	echo    "2/8 Generate CA certificate" &&
	echo -e "---------------------------\n" &&
	echo "How many days should this certificate be valid ?" &&
	echo -n "Number of days: " &&
	read cert_days &&
	openssl req -new -x509 -days $cert_days -key $CA_PATH/CA.key > $CA_PATH/CA.crt
}

function create_key {
	clear &&

	TXT="$2 Create the key $1"
	echo $TXT;
	underline "$TXT"

	openssl genrsa 2048 > $1 &&
	echo -e "==> Key $1 created\n\n"
}

# arg1 = key (in)
# arg2 = cert.csr (out)
# arg3 = 2/8 (feedback)
function create_certificate_signing_request {
	# No clear here so as to see that the key
	# has been created for the server/client
	TXT="$3 Fill in the informations to create a certificate signing request for $2"
	echo $TXT &&
	underline "$TXT" &&

	openssl req -new -key $1 > $2
}

function sign_cert {
	clear &&
	TXT="$2 Sign the request certificate $1" &&
	echo $TXT;
	underline "$TXT";

	echo "How many days should this certificate be valid ?" &&
	echo -n "Number of days: " &&
	read cert_days &&

	certsigned_out=${1:0:$((${#1}-4))}
	openssl x509 -req -in $1 -out $certsigned_out.crt -CA $CA_PATH/CA.crt\
	-CAkey $CA_PATH/CA.key -CAcreateserial -CAserial CA.srl -days $cert_days
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

if [ ! -d "$CA_PATH" ]; then
	mkdir $CA_PATH
else
	exit_or_continue "CA" && rm -f $CA_PATH/*
fi

if [ ! -d "$SERVER_PATH" ]; then
	mkdir $SERVER_PATH
else
	exit_or_continue "server" && rm -f $SERVER_PATH/*
fi

if [ ! -d "$CLIENT_PATH" ]; then
	mkdir $CLIENT_PATH
else
	exit_or_continue "client" && rm -f $CLIENT_PATH/*
fi



###############################################
# CA
###############################################
create_ca_key || failure "CA key not created";
chmod 400 $CA_PATH/*.key
create_ca_cert || failure "CA cert not created";
###############################################



###############################################
# Server
###############################################
create_key $SERVER_PATH/rw_serv.key 3/8 || failure "Could not create the server's key";
chmod 400 $SERVER_PATH/*.key
create_certificate_signing_request $SERVER_PATH/rw_serv.key $SERVER_PATH/rw_serv.csr 4/8 || failure "Could not create a certificate signing request";

sign_cert $SERVER_PATH/rw_serv.csr 5/8 && rm -f CA.srl $SERVER_PATH/rw_serv.csr || failure "CA could not sign server certificate request";

echo -e "\n==> Server certificate ready."
###############################################



###############################################
# Client
###############################################
create_key $CLIENT_PATH/rw_client.key 6/8 || failure "Could not create the client's key";
chmod 400 $CLIENT_PATH/*.key
create_certificate_signing_request $CLIENT_PATH/rw_client.key $CLIENT_PATH/rw_client.csr 7/8 || failure "Could not request a signed certificate";

sign_cert $CLIENT_PATH/rw_client.csr 8/8 && rm -f CA.srl $CLIENT_PATH/rw_client.csr || failure "CA could not sign client certificate request";

echo -e "\n==> Client certificate ready.\n";
###############################################


echo "Results :";
echo "---------";

echo "CA's key and certificate created with success";
echo "Server and Client's key and certificate created and signed by the CA with success";

exit 0;
