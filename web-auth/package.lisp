(in-package #:cl-user)

(defpackage #:jfh-web-auth
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core))
  (:export
   #:define-protected-page
   #:authenticated-user
   #:use-web-auth
   #:web-auth-pages
   #:*web-auth-pages*
   #:show-auth-failure
   #:find-user-info
   #:login-page
   #:logout-page
   #:signup-page
   #:validate-signup-parameters
   #:establish-user-session
   #:get-authenticated-user))
