(in-package #:cl-user)

(defpackage #:jfh-certs-web-app
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core) (#:auth #:jfh-web-auth))
  (:export
   #:*web-configuration*
   #:*static-paths-maps*
   #:setup-dispatch-for-all-html-files
   #:signup-page
   #:login-page
   #:find-user-info
   #:show-auth-failure
   #:on-auth-hook))
