(in-package #:jfh-certs-main)

(defparameter *web-application* nil)
(defparameter *actual-swank-port* nil)

(defun register-web-auth-functions ()
  "Register functions using provided class to enable web-auth features."
  (make-instance 'auth:web-auth-pages
                 :on-auth-hook 'web-app:on-auth-hook
		 :signup-page 'web-app:signup-page
		 :login-page 'web-app:login-page
		 :find-user-info 'jfh-app-core:find-user-info
		 :find-secure-user-info 'jfh-app-core:find-secure-user-info
		 :show-auth-failure 'web-app:show-auth-failure))

(defun application-start ()
  "Use this to start the application."
  (flet ((map-static-paths ()
	   (mapc
	    (lambda (pair)
	      (jfh-web-core:add-static-path-map (car pair) (cadr pair)))
	    jfh-certs-web-app:*static-paths-maps*)))

    (map-static-paths)
    (web-app:setup-dispatch-for-all-html-files)
    
    (let ((web-application (jfh-web-core:web-application-shell)))

      (setf web-app:*web-configuration* (web:web-configuration web-application))
      (auth:use-web-auth (register-web-auth-functions))
      
      (setf *actual-swank-port* (jfh-app-core:start-swank (jfh-app-core:application-configuration (jfh-web-core:web-configuration web-application))))

      (setf *web-application* web-application))))

(defun application-stop (&optional (stop-swank t) (web-application *web-application*) (actual-swank-port *actual-swank-port*))
  "Use this to stop the application. Stopping swank is optional."
  (jfh-web-core:stop-web-app web-application)
  (if (and stop-swank actual-swank-port)
      (jfh-app-core:stop-swank (jfh-app-core:application-configuration web-app:*web-configuration*))))
