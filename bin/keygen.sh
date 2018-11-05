sudo -v
cd /etc/httpd/conf || exit
openssl req -new -nodes > "$1.csr "
openssl rsa -in privkey.pem -out "$1.key"
openssl x509 -in "$1.csr" -out "$1.crt" -req -signkey "$1.key" -days 365
