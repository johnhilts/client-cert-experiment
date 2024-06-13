(cl:in-package #:cl-user)

(defun load-local-certs ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/certs/")
  (push #p"/home/jfh/code/lisp/source/certs/" asdf:*central-registry*)
  (asdf:load-system "jfh-certs-main")
  ;; (in-package #:jfh-certs-main)
  )
