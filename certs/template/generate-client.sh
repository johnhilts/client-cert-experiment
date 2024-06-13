openssl genrsa -out client.key 2048
openssl req -new -key client.key -out client.csr -config san.cnf
openssl x509 -req -in client.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out client.crt -days 500 -sha256 -extfile san.cnf -extensions req_ext
openssl pkcs12 -export -out client.p12 -inkey client.key -in client.crt -certfile ca.crt
openssl x509 -outform der -in client.crt -out client.der
