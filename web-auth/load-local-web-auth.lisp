(cl:in-package #:cl-user)

(defun load-local-web-auth ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/certs/web-auth/")
  (push #p"/home/jfh/code/lisp/source/certs/web-auth/" asdf:*central-registry*)
  (asdf:load-system "jfh-web-auth")
  ;; (in-package #:jfh-web-auth)
  )
