(cl:in-package #:asdf-user)

(defsystem #:jfh-certs-main
  :description "PoC for using certs with hunchentoot"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:jfh-certs-web-app #:jfh-web-core #:jfh-app-core #:jfh-utility)
  :components ((:file package)
               (:file application)
               (:file main)))

