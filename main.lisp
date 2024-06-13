(in-package #:cl-user)

(defun jfh-app-main ()
  (jfh-certs-main::application-start)
  (sb-impl::toplevel-repl nil))
