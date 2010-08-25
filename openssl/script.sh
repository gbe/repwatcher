#!/bin/bash

#set -x
#set -e

function create_ca_key {
	echo "Create the CA key" &&
	openssl genrsa -aes256 1024 > CA/ca.key
}

function create_ca_cert {
	clear &&	
	echo -e "Create the CA cert from the CA key\n" &&
	echo "How many days should this certificate be valid ?" &&
	echo -n "Number of days: " &&
	read cert_days &&
	openssl req -new -x509 -days $cert_days -key CA/ca.key > CA/ca.crt
}

function create_key {
	clear &&
	echo -e "Create the key $1\n" &&
	openssl genrsa 1024 > $1 &&
	echo -e "==> Key $1 created\n\n"
}

#arg1 = key (in)
#arg2 = cert.csr (out)
function request_signed_cert {
	# No clear here so as to see that the key
	# has been created for the server/client
	openssl req -new -key $1 > $2
}

function sign_cert {
	clear &&
	echo -e "Sign the request certificate $1\n" &&
	certsigned_out=${1:0:$((${#1}-4))}
	openssl x509 -req -in $1 -out $certsigned_out.crt -CA CA/ca.crt\
	-CAkey CA/ca.key -CAcreateserial -CAserial ca.srl
}

function failure {
	echo "Failure. $1" &&
	exit 1
}

rm -rf CA server

if [ ! -d "CA" ]; then
	mkdir CA
fi

if [ ! -d "server" ]; then
	mkdir server
fi

create_ca_key || failure "CA key not created";

create_ca_cert || failure "CA cert not created";

create_key server/rw_serv.key || failure "Could not create the server's key";

request_signed_cert server/rw_serv.key server/rw_serv.csr || failure "Couldn't request a signed certificate";

sign_cert server/rw_serv.csr && rm -f ca.srl server/rw_serv.csr || failure "CA could't sign serveur certificate";

echo -e "\n==> Server certificate ready."

exit 0;
