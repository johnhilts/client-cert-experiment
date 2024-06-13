(cl:in-package #:asdf-user)

(defsystem #:jfh-web-auth
  :description "Utility to do things with notes made on a kindle e-reader."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:cl-json #:cl-who #:jfh-utility #:jfh-web-core #:jfh-app-core)
  :components ((:file package)
	       (:file macros)
               (:file web-auth-protocol)
	       (:file web-auth)
               (:file pages)))

