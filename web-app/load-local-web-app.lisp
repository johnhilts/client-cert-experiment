(cl:in-package #:cl-user)

(defun load-local-web-app ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/certs/web-app/")
  (push #p"/home/jfh/code/lisp/source/certs/web-app/" asdf:*central-registry*)
  (asdf:load-system "jfh-certs-web-app")
  ;; (in-package #:jfh-certs-web-app)
  )
