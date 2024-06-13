;;;; Notes
(cl:in-package #:jfh-certs-web-app)

(defparameter *notes* (make-hash-table :test 'equal)
  "Key: User ID, Value: sequence of user's notes.")

(defun get-notes-path (authenticated-user-id)  ;; todo can this be made into a method?
  "Get the path of user's notes."
  (let* ((configuration (jfh-app-core:application-configuration *web-configuration*))
	 (user-path (jfh-app-core:find-user-path (jfh-app-core:make-application-user authenticated-user-id) configuration))
	 (note-file-name "certs")
         (path (make-pathname :name note-file-name
                                  :type "txt"
                                  :defaults (truename user-path))))
    (values path note-file-name)))

(defun read-user-notes (user-id user-notes-path)
  "Fetch user's notes and save in global dictionary."
  (let ((notes "These are some dummy notes."))
    (setf (gethash user-id *notes*) notes)))
