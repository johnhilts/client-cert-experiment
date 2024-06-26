;;;; start, stop web-app
(cl:in-package #:jfh-web-core)

(defvar *web-application*)

(defmethod print-object ((web-configuration web-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (web-configuration stream :type t)
    (with-accessors
          ((http-port http-port)
           (ssl-port ssl-port)
	   (static-root static-root)
           (application-configuration jfh-app-core:application-configuration))
        web-configuration
      (format stream
              ;; "~a, ~a, ~a" http-port ssl-port application-configuration
	      ;; "~:[~:;Swank Port: ~:*~d~]~:[~:;, Swank Interface: ~:*~a~]~:*~:[~:;, ~]HTTP Port: ~d, ~:[~:;SSL Port: ~:*~d, ~]Settings File: ~s, User Path: ~s"
	      ;; swank-port swank-interface http-port ssl-port settings-file-path user-path-root
              "~A, ~:[~:;HTTP Port: ~:*~D, ~]~:[~:;SSL Port: ~:*~D, ~]Static root path: ~S" application-configuration http-port ssl-port static-root))))

(defmethod print-object ((web-application web-application) stream)
  "Print web application."
  (print-unreadable-object (web-application stream :type t)
    (with-accessors ((hunchentoot-ssl-acceptor hunchentoot-ssl-acceptor) (hunchentoot-acceptor hunchentoot-acceptor) (web-configuration web-configuration))
        web-application
      (format stream
	      "Hunchentoot SSL Acceptor: ~a, Hunchentoot Acceptor: ~a, Configuration: ~a" hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration))))

(defun make-web-configuration (&optional (ssl-port nil) (http-port 8080) (static-root ""))
  "Get configuration info from the file system and hydrate web-configuration object.
Input: default configuration values.
Output: web-configuration object."
  (let* ((app-configuration (jfh-app-core:make-application-configuration))
         (call-back #'(lambda (settings)
			(if settings
			    (make-instance 'web-configuration
					   :ssl-port (getf settings :ssl-port)
					   :http-port (getf settings :http-port)
					   :static-root (getf settings :static-root)
                                           :application-configuration app-configuration)
	                    (make-instance 'web-configuration
					   :ssl-port ssl-port
					   :http-port http-port
					   :static-root static-root
                                           :application-configuration app-configuration)))))
    (jfh-utility:fetch-or-create-data (jfh-app-core:settings-file-path app-configuration) call-back)))

(defmethod start-hunchentoot ((web-configuration web-configuration))
  "start or re-start the hunchentoot web server"
  (flet ((make-acceptor-instances ()
           (with-accessors ((ssl-port ssl-port) (http-port http-port)) web-configuration
             (values
              (make-instance 'my-ssl-acceptor :port ssl-port :ssl-privatekey-file #P"./certs/set5/server.key" :ssl-certificate-file #P"./certs/set5/server.crt")
              (make-instance 'http-to-https-acceptor :port http-port :ssl-port ssl-port))
             ;; (if ssl-port
             ;;     ;; (make-instance 'my-ssl-acceptor :port ssl-port :ssl-privatekey-file #P"./certs/server.key" :ssl-certificate-file #P"./certs/server.crt")
             ;;     (make-instance 'my-ssl-acceptor :port ssl-port :ssl-privatekey-file #P"./certs/set5/server.key" :ssl-certificate-file #P"./certs/set5/server.crt")
             ;;     (make-instance 'tbnl:easy-acceptor :port http-port))
             ))
         (start-hunchentoot-by-http-protocol-type (acceptor-instance acceptor-type) ;; TODO: "acceptor-instance" is redundant!!
           (prog1
               (restart-case (tbnl:start acceptor-instance)
	         (skip-hunchentoot-start ()
                   :report "Skip starting Web Server (hunchentoot)."
                   (ecase acceptor-type
                    (ssl (hunchentoot-ssl-acceptor *web-application*))
                    (http (hunchentoot-acceptor *web-application*))))
	         ;; (use-different-port ()
	         ;;   :report "Use a different port - will increment by 1 from the configured port number."
	         ;;   ;; (format nil "Use a different port - change from ~d to ~d" (tbnl:acceptor-port acceptor-instance) (1+ (tbnl:acceptor-port acceptor-instance))))
	         ;;   (with-accessors ((http-port http-port) (ssl-port ssl-port)) web-configuration
	         ;; 	(start-hunchentoot (make-web-configuration (if ssl-port (1+ ssl-port) nil) (if http-port (1+ http-port) nil)))))
	         )
             (format t "~&instance: ~A~%" acceptor-instance))))
    (multiple-value-bind (ssl-acceptor-instance acceptor-instance)
        (make-acceptor-instances)
      (prog1
	  (list
           (start-hunchentoot-by-http-protocol-type ssl-acceptor-instance 'ssl)
           (start-hunchentoot-by-http-protocol-type acceptor-instance 'http))
        (format t "~&ssl: ~A, reg: ~A~%" ssl-acceptor-instance acceptor-instance)
        ;; (break)
	(format t "~&hunchentoot started~%")))))

(defun %make-web-application-core (hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration)
  "Constructor for web-application"
  (make-instance 'web-application
		 :hunchentoot-acceptor hunchentoot-acceptor
		 :hunchentoot-ssl-acceptor hunchentoot-ssl-acceptor
		 :web-configuration web-configuration))

(defmethod make-web-application ((hunchentoot-ssl-acceptor tbnl:easy-ssl-acceptor) (hunchentoot-acceptor tbnl:easy-acceptor) (web-configuration web-configuration))
  "Constructor for web-application - handles all parameters."
  (%make-web-application-core hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration))

(defmethod make-web-application ((hunchentoot-ssl-acceptor tbnl:easy-ssl-acceptor) (hunchentoot-acceptor (eql nil)) (web-configuration web-configuration))
  "Constructor for web-application - accepts nil for http."
  (%make-web-application-core hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration))

(defmethod make-web-application ((hunchentoot-ssl-acceptor (eql nil)) (hunchentoot-acceptor tbnl:easy-acceptor) (web-configuration web-configuration))
  "Constructor for web-application - accepts nil for ssl."
  (%make-web-application-core hunchentoot-ssl-acceptor hunchentoot-acceptor web-configuration))

(defparameter *static-path-maps* ())

(defun add-static-content-handlers ()
  "Add handlers for provided static content web/path mappings."
  (mapc
   (lambda (mapping)
     (push
      (tbnl:create-static-file-dispatcher-and-handler (car mapping) (cdr mapping))
      tbnl:*dispatch-table*))
   *static-path-maps*)
  ;; (pushnew (tbnl:create-static-file-dispatcher-and-handler
  ;;        "/favicon.ico" "ez-favicon.ico") *dispatch-table*)
  ;; (push (tbnl:create-static-file-dispatcher-and-handler
  ;;        "/styles.css" "static/styles.css") *dispatch-table*)
  )

(defun add-static-path-map (web-path physical-path)
  "add mapping pairs to be used to expose static assets from the web server."
  (pushnew
   (cons web-path physical-path)
   *static-path-maps*
   :key #'car
   :test #'string=))

(defmethod start-web-app ((web-configuration web-configuration))
  "Input: application-configuration object and path maps for static assets. Output: web-application object. This will start the web application running on top of hunchentoot."
  (setf tbnl:*session-max-time* (* 24 7 60 60))
  (setf tbnl:*rewrite-for-session-urls* nil)
  (add-static-content-handlers)
  (destructuring-bind
      (ssl-acceptor acceptor)
      (start-hunchentoot web-configuration)
    (format t "~&ssl: ~A, reg: ~A~%" ssl-acceptor acceptor)
    ;; (break)
    (make-web-application ssl-acceptor acceptor web-configuration)))
;; how to find: (find-method #'start-web-app nil (list (find-class 'application-configuration)))

(defmethod stop-hunchentoot ((web-application web-application))
  "Input: web-application. Stop hunchentoot web-server via the provided web-application object."
  (flet ((stop-hunchentoot-by-http-protocol-type (acceptor)
           (restart-case (tbnl:stop acceptor)
             (skip-hunchentoot-stop ()
               :report "Skip stopping Web Server (hunchentoot)."
               acceptor))))
    (stop-hunchentoot-by-http-protocol-type (hunchentoot-acceptor web-application))
    (stop-hunchentoot-by-http-protocol-type (hunchentoot-ssl-acceptor web-application))))

(defmethod stop-web-app ((web-application web-application))
  "Input: web-application and application-configuration objects. Output: #:web-app-stopped. This will stop the web application. The HTTP port will be released"
  (stop-hunchentoot web-application)
  '#:web-app-stopped)

(defun web-application-shell ()
  "Use this to start the web application."
  (let* ((web-configuration (make-web-configuration)))
    (setf *web-application* (start-web-app web-configuration))))
