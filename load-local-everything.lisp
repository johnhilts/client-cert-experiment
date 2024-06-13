(cl:in-package #:cl-user)

(defun load-local-everything ()
  (load "/home/jfh/code/lisp/source/certs/utility/load-local-utility.lisp")
  (load-local-utility)
  (print "utilty loaded")

  (load "/home/jfh/code/lisp/source/certs/app-core/load-local-app-core.lisp")
  (load-local-app-core)
  (print "app-core loaded")

  (load "/home/jfh/code/lisp/source/certs/web-core/load-local-web-core.lisp")
  (load-local-web-core)
  (print "web-core loaded")

  (load "/home/jfh/code/lisp/source/certs/web-auth/load-local-web-auth.lisp")
  (load-local-web-auth)
  (print "web-auth loaded")

  (load "/home/jfh/code/lisp/source/certs/web-app/load-local-web-app.lisp")
  (load-local-web-app)
  (print "web-app loaded")

  (swank:set-default-directory "/home/jfh/code/lisp/source/certs/")
  (push #p"/home/jfh/code/lisp/source/certs/" asdf:*central-registry*)
  (asdf:load-system "jfh-certs-main")
  (print "main loaded")
  ;; (in-package #:jfh-certs-main)
  )
