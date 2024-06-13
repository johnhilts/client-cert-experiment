openssl genrsa -out ca.key 4096
openssl req -x509 -new -nodes -key ca.key -sha256 -days 1024 -out ca.crt -config ca.cnf
openssl genrsa -out server.key 2048
openssl req -new -key server.key -out server.csr -config san.cnf
openssl x509 -req -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out server.crt -days 500 -sha256 -extfile san.cnf -extensions req_ext
openssl genrsa -out client.key 2048
openssl req -new -key client.key -out client.csr -config san.cnf
openssl x509 -req -in client.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out client.crt -days 500 -sha256 -extfile san.cnf -extensions req_ext
openssl verify -CAfile ca.crt server.crt
openssl x509 -in server.crt -noout -issuer
openssl verify -CAfile ca.crt -untrusted intermediate.crt server.crt
openssl pkcs12 -export -out client.p12 -inkey client.key -in client.crt -certfile ca.crt
openssl x509 -outform der -in client.crt -out client.der
openssl x509 -outform der -in server.crt -out server.der
openssl x509 -outform der -in ca.crt -out ca.der
