(cl:in-package #:cl-user)

(defun load-local-app-core ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/certs/app-core/")
  (push #p"/home/jfh/code/lisp/source/certs/app-core/" asdf:*central-registry*)
  (asdf:load-system "jfh-app-core")
  ;; (in-package #:jfh-app-core)
  )
