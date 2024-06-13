(cl:in-package #:cl-user)

(defun load-local-utility ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/certs/utility/")
  (push #p"/home/jfh/code/lisp/source/certs/utility/" asdf:*central-registry*)
  (asdf:load-system "jfh-utility")
  ;; (in-package #:jfh-utility)
  )
