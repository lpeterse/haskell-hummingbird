#!/bin/bash

set -e

openssl genrsa -out private.pem 2048
openssl rsa -in private.pem -outform PEM -pubout -out public.pem
openssl req -new -sha256 -key private.pem -out csr.csr -subj "/CN=$1"
openssl req -x509 -sha256 -days 3650 -key private.pem -in csr.csr -out certificate.pem
openssl x509 -text -in certificate.pem