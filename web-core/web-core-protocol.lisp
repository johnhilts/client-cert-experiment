;;;; protocol related to web core
(cl:in-package #:jfh-web-core)

(defclass web-configuration ()
  ((%http-port
    :reader http-port
    :initarg :http-port)
   (%ssl-port
    :reader ssl-port
    :initarg :ssl-port)
   (%static-root
    :reader static-root
    :initarg :static-root)
   (%application-configuration
    :reader jfh-app-core:application-configuration
    :initarg :application-configuration))
  (:documentation "Application configurations."))

(defclass web-application ()
  ((%hunchentoot-acceptor
    :reader hunchentoot-acceptor
    :initarg :hunchentoot-acceptor)
   (%web-configuration
    :reader web-configuration
    :initarg :web-configuration))
  (:documentation "Web application."))

(defgeneric start-hunchentoot (web-configuration)
  (:documentation "Input: application-configuration. Start hunchentoot web-server with the provided configuration settings."))

(defgeneric start-web-app (web-configuration)
  (:documentation "Input: application-configuration object and path maps for static assets. Output: web-application object. This will start the web application running on top of hunchentoot, and optionally start swank."))
;; (documentation 'start-web-app 'function)

(defgeneric stop-hunchentoot (web-application)
  (:documentation "Input: web-application. Stop hunchentoot web-server via the provided web-application object."))

(defgeneric stop-web-app (web-application)
  (:documentation "Input: web-application objects. Output: #:web-app-stopped. This will stop the web application. The HTTP port will be released."))
;; (documentation 'stop-web-app 'function)

(defgeneric make-web-application (tbnl:easy-acceptor web-configuration)
  (:documentation "Input: hunchentoot easy-acceptor, application-configuration (default settings) object. Output web-application object."))

(defmethod tbnl:process-request :around ( (*request* tbnl:request))
  ;; (setf *sneaky-acceptor* *acceptor*)
  ;; (setf *sneaky-request* *request*)
  ;; (break)
  (when (next-method-p)
    (call-next-method)))

(defparameter *sneaky-acceptor* nil)
(defparameter *sneaky-request* nil)
(defparameter *sneaky-client-cert* nil)

(defmethod tbnl:handle-request :around ((*acceptor* tbnl:acceptor) (*request* tbnl:request))
  ;; (let ((tbnl::*hunchentoot-stream* (tbnl::content-stream *request*)))
  (setf *sneaky-acceptor* *acceptor*)
  (setf *sneaky-request* *request*)
  ;; (break)
  (setf *sneaky-client-cert* (tbnl:get-peer-ssl-certificate)) ;; get the SAP
  ;; )
  (when (next-method-p)
    (call-next-method)))

(define-condition cert-file-missing (file-error)
  ()
  (:report (lambda (condition stream) (format stream "Unable to find certificate folder: ~A." (file-error-pathname condition)))))

(defclass my-ssl-acceptor (tbnl:easy-ssl-acceptor) ())

#|
To allow more complex recovery protocols, restarts can take arbitrary arguments, which are passed in the call to INVOKE-RESTART.
You can provide support for both the recovery strategies I just mentioned by adding two restarts to parse-log-entry, each of which takes a single argument.
One simply returns the value it’s passed as the return value of parse-log-entry, while the other tries to parse its argument in the place of the original log entry.
(defun parse-log-entry (text) (if (well-formed-log-entry-p text) (make-instance 'log-entry ...) (restart-case (error 'malformed-log-entry-error :text text) (use-value (value) value) (reparse-entry (fixed-text) (parse-log-entry fixed-text)))))
The name USE-VALUE is a standard name for this kind of restart.
Common Lisp defines a restart function for USE-VALUE similar to the skip-log-entry function you just defined.
So, if you wanted to change the policy on malformed entries to one that created an instance of malformed-log-entry,
you could change log-analyzer to this (assuming the existence of a malformed-log-entry class with a :text initarg):
(defun log-analyzer () (handler-bind ((malformed-log-entry-error #'(lambda (c) (use-value (make-instance 'malformed-log-entry :text (text c)))))) (dolist (log (find-all-logs)) (analyze-log log))))
|#

#| from pcl book
(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry ...)
      (restart-case
          (error 'malformed-log-entry-error :text text)
        (use-value (value) value)
        (reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

(defun log-analyzer ()
  (handler-bind
      ((malformed-log-entry-error #'(lambda (c) (use-value (make-instance 'malformed-log-entry :text (text c))))))
    (dolist (log (find-all-logs))
(analyze-log log))))
|#

(defun my-wrapper (&optional (default-path "./certs/set3/"))
  (handler-bind
      ((cert-file-missing #'(lambda (c) (use-value c)))) ;; what's even the point of something like this??
    (get-my-cert-path default-path)))

(defun get-my-cert-path (&optional (default-path "./certs/set5/"))
  (restart-case (if (probe-file default-path)
                    default-path
                    (error 'cert-file-missing :pathname default-path "Couldn't find certificate path ~S:" default-path))
    (use-value (value)
      :report
      (lambda (s) ;Argument s is a stream
        (format s "Specify a value of ~S to use this time." default-path))
      :interactive
      (lambda ()
        (format *query-io* "Enter a different path - Example: ~A: " default-path)
        (finish-output *query-io*)      ; necessary for tunnels
        (ignore-errors (list (read-line *query-io*))))
      value)))


(defun hash-array->string (array)
  (let ((res ""))
    (loop for i across array do
      (setf res
            (concatenate 'string
                         res
                         (format nil "~2,'0x" i))))
    res))

(defmethod tbnl:initialize-connection-stream ((acceptor my-ssl-acceptor) stream)
  ;; attach SSL to the stream if necessary
  (let ((my-cert-path (get-my-cert-path)))
    ;; (break) ;; not getting hit?!?
    (let ((ctx (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-peer+
                                    :verify-depth 1
                                    :verify-location (format nil "~Aca.crt" my-cert-path) ;; client-certificates-directory
                                    :certificate-chain-file (format nil "~Aca.crt" my-cert-path) ;; "path/to/ca_cert.pem"
                                    )))
      ;; (break)
      (print "make server stream ...")
      (cl+ssl:with-global-context (ctx :auto-free-p t)
        (let* ((server-stream (cl+ssl:make-ssl-server-stream
                               ;; (usocket:socket-listen "192.168.1.18" 5098)
                               stream
                               :certificate (format nil "~Aserver.crt" my-cert-path) ;; "path/to/server_cert.pem"
                               :key (format nil "~Aserver.key" my-cert-path) ;; "path/to/server_key.pem"
                               ;; :password "."
                               ;; :context ctx
                               ))
               (client-certificate (cl+ssl:ssl-stream-x509-certificate server-stream))
               (client-cert-fingerprint (cl+ssl:certificate-fingerprint client-certificate :sha256)))
          (format t "~&cert: ~A~%" client-certificate)
          (format t "~&fingerprint: ~A~%" client-cert-fingerprint)
          ;; (let ((data (make-list 2)))
          ;;   (read-sequence data server-stream)
          ;;   (format t
          ;;           "Server got from client (identified by ~s): ~s~%"
          ;;           (hash-array->string client-cert-fingerprint)
          ;;           (coerce (mapcar #'code-char data)
          ;;                   'string))
          ;;   (cl+ssl:x509-free client-certificate))
          ;; Your server loop here, where you accept connections and handle requests
          ;; (print "ready to call next method")
          ;; (call-next-method acceptor
          ;;                   server-stream)
          server-stream)
        ;; (call-next-method acceptor
        ;;                 (cl+ssl:make-ssl-server-stream stream
        ;;                                                :certificate (acceptor-ssl-certificate-file acceptor)
        ;;                                                :key (acceptor-ssl-privatekey-file acceptor)
        ;;                                                :password (acceptor-ssl-privatekey-password acceptor)))
        ))))


