;;;; functions related to an application user.
(cl:in-package #:jfh-app-core)

(defmethod initialize-instance :after ((application-user application-secure-user) &key)
  "Initializations:
- Encrypt the user password. This is meant to prevent the plain text password from being in memory.
- Set the User ID to a unique ID."
  (let ((user-id #1=(slot-value application-user '%user-id))
        (password #2=(slot-value application-user '%user-password)))
    (when (zerop (length user-id))
      (setf #1# (jfh-utility:generate-unique-token))
      (setf #2# (jfh-utility:hash-password password)))))

(defun make-application-secure-user (user-login user-password)
  "Constructor for application-secure-user."
  (make-instance 'application-secure-user :user-login user-login :user-password user-password))

(defun make-application-user (user-id)
  "Constructor for application-user."
  (make-instance 'application-user :user-id user-id :user-login ""))

(defmethod print-object ((application-user application-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login)) application-user
      (format stream
	      "User ID: ~A, User Login: ~S" user-id user-login))))

(defmethod print-object ((application-user application-meta-user) stream)
  "Print application user."
  (print-unreadable-object (application-user stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login) (create-date create-date) (disable disable)) application-user
      (format stream
	      "User ID: ~A, User Login: ~S, Created: ~A, Disabled: ~:[false~;true~]" user-id user-login create-date disable))))

(defmethod find-user-path ((application-user application-user) (application-configuration application-configuration))
  "Input: application-user and app-configuration. Output: user path."
  (with-accessors ((user-path-root user-path-root)) application-configuration
    (with-accessors ((user-id user-id)) application-user
      (format nil "~A~A/" user-path-root user-id))))

(defmethod save-user (file-name user-info-list (application-user application-user) (application-configuration application-configuration))
  "Input: file-name, user info list (not a class), application-user and app-configuration. Output: user info list. Persist application user info."
  (let ((user-info-file-path (format nil "~A~A" (find-user-path application-user application-configuration) file-name)))
    (jfh-utility:write-complete-file user-info-file-path user-info-list)))

(defmethod save-application-user ((application-user application-meta-user) (application-configuration application-configuration))
  "Input: application-meta-user and app-configuration. Output: serialized application-meta-user. Persist application user info."
  (let ((file-name "user.sexp")
        (user-info-list (list
                         :user-id (user-id application-user)
                         :user-login (user-login application-user)
                         :create-date (create-date application-user)
                         :disable (disable application-user))))
    (save-user file-name user-info-list application-user application-configuration)
    (when (next-method-p)
      (call-next-method))))

(defmethod save-application-user ((application-user application-secure-user) (application-configuration application-configuration))
  "Input: application-secure-user and app-configuration. Output: serialized application-user. Persist application user info."
  (let ((file-name "hash.sexp")
        (user-info-list (list
                         :user-password (user-password application-user))))
    (save-user file-name user-info-list application-user application-configuration)))

(defmethod print-object ((user-index-entry user-index-entry) stream)
  "Print user index entry."
  (print-unreadable-object (user-index-entry stream :type t)
    (with-accessors ((user-id user-id) (user-login user-login)) user-index-entry
      (format stream
	      "User ID: ~A, User Name: ~S" user-id user-login))))

(defmethod make-user-index-entry ((application-user application-user))
  "Input: application-user. Output: user index entry."
  (make-instance 'user-index-entry
		 :user-login (user-login application-user)
		 :user-id (user-id application-user)))

(defmethod user-index-entry->list ((user-index-entry user-index-entry))
  "Input: user index entry. Output: regular list. Conversion function."
  (list
   :user-id (user-id user-index-entry)
   :user-login (user-login user-index-entry)))

(defun get-user-index-file-path (user-path-root)
  (format nil "~Auser-index.sexp" user-path-root))

(defmethod save-new-application-user ((application-user application-meta-user) (application-configuration application-configuration))
  "Input: application-meta-user and app-configuration. Output: application-user. Persist application user info."
  (let* ((user-path-root (user-path-root application-configuration))
         (user-index-file-path (get-user-index-file-path user-path-root)))
    (flet ((callback (user-index)
             (push (user-index-entry->list (make-user-index-entry application-user)) user-index)
             (jfh-utility:write-complete-file user-index-file-path user-index)))
      (ensure-directories-exist user-path-root)
      (jfh-utility:fetch-or-create-data user-index-file-path #'callback)
      (ensure-directories-exist (find-user-path application-user application-configuration))
      (save-application-user application-user application-configuration))))

(defun user-entry->application-user (user-entry)
  "Input: p-list. Output: application-user."
  (make-instance 'application-meta-user
		 :user-id (getf user-entry :user-id)
		 :user-login (getf user-entry :user-login)
		 :create-date (getf user-entry :create-date)
		 :disable (getf user-entry :disable)))

(defun user-entry->application-secure-user (application-user user-entry) ;; TODO convert to defmethod
  "Input: p-list. Output: application-secure-user."
  (make-instance 'application-secure-user
		 :user-id (user-id application-user)
		 :user-login (user-login application-user)
		 :user-password (getf user-entry :user-password)))

(defun read-user-info (user-id file-name)
  "read user info from user-id/user.sexp The guid-like user ID is needed to find the folder."
  (let ((user-info (make-application-user user-id)))
    (jfh-utility:read-complete-file (format nil "~A/~A" (find-user-path user-info (make-application-configuration)) file-name))))

(defun find-user-info-old (user-login)
  "Search for user info in file system."
  (let* ((user-index-entry (find-user-index-entry user-login (make-application-configuration))) ;; TODO don't remake the configuration!!
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (user-entry->application-user (read-user-info user-id "user.sexp")))))

(defun find-user-info (user-login)
  "Search for user info in file system."
  (let* ((user-index-entry (find-user-index-entry user-login (make-application-configuration))) ;; TODO don't remake the configuration!!
         (user-id (getf user-index-entry :user-id)))
    (when user-id
      (make-instance-from-data-store 'application-meta-user (list :user-id '? :user-login '? :create-date '? :disable '?) user-id))))

(defun find-secure-user-info-old (user-login)
  "Search for secure user info in file system."
  (let* ((application-user (find-user-info user-login)))
    (when application-user
      (user-entry->application-secure-user application-user (read-user-info (user-id application-user) "hash.sexp")))))

(defun find-secure-user-info (user-login)
  "Search for secure user info in file system."
  (let* ((application-user (find-user-info user-login)))
    (when application-user
      (let ((user-id (user-id application-user)))
        (make-instance-from-data-store 'application-secure-user (list :user-id user-id :user-login (user-login application-user) :user-password '?) user-id)))))

(defmethod find-user-index-entry (user-login (application-configuration application-configuration))
  "Input: User ID and app-configuration. Output: user index entry."
  (let* ((user-path-root (user-path-root application-configuration))
         (user-index-file-path (get-user-index-file-path user-path-root))
	 (user-index (jfh-utility:fetch-or-create-data user-index-file-path)))
    (find-if (lambda (entry) (string= (getf entry :user-login) user-login)) user-index)))

(defgeneric get-user-data-store-location (id application-configuration))

(defmethod get-user-data-store-location (user-id (application-configuration application-configuration))
  "Input: ID and app-configuration. Output: file path."
  (with-accessors ((user-path-root user-path-root)) application-configuration
    (format nil "~A~A/" user-path-root user-id)))
;; example: (jfh-app-core::get-user-data-store-location "abc-123" (jfh-app-core:application-configuration (jfh-web-core:web-configuration *WEB-APPLICATION*)))

(defun find-and-replace-missing-values (initargs fill-in-values)
  "Input: 1st plist, where some values are '?, 2nd plist which has fill-in values for '? in the 1st plist. Ouptput: the 1st plist modified to replace '? with a fill-in-value."
  (loop for initarg in initargs
        when (and
              (symbolp (getf initargs initarg))
              (string= "?" (symbol-name (getf initargs initarg))))
          do
             (setf (getf initargs initarg) (getf fill-in-values initarg)))
  initargs)

(defparameter *application-configuration* nil)
;; (setf jfh-app-core::*application-configuration*  (jfh-app-core:application-configuration (jfh-web-core:web-configuration *WEB-APPLICATION*)))

(defun make-instance-from-data-store (class-name initargs id &optional (configuration *application-configuration*) (get-data-store-location #'get-user-data-store-location))
  "Input: class and its initarg names+values and an ID. Output: object using make-instance `class-name` with data from parameters + data store."
  (let* ((file-name (format nil "~(~A~).sexp" class-name))
         (file-path (funcall get-data-store-location id configuration))
         (entry (jfh-utility:read-complete-file (format nil "~A~A" file-path file-name)))
         (initargs-no-missing-values (find-and-replace-missing-values initargs entry)))
    (apply #'make-instance class-name initargs-no-missing-values)))

(defun make-instance-from-data-store-index (index-name class-name initargs &optional (configuration *application-configuration*))
  "Input: class and its initarg names+values and an ID. Output: object using make-instance `class-name` with data from parameters + data store."
  (let* ((file-name (format nil "~(~A~).sexp" index-name))
         (file-path (format nil "~A~A" (user-path-root configuration) file-name))
         (complete-index (jfh-utility:read-complete-file file-path))
         ;; (initargs-no-missing-values (find-and-replace-missing-values initargs entry))
         (initarg-keys (loop for keys = initargs then (cddr keys)
                             for key = (car keys)
                             while keys
                             collect key))
         (index-entry (find-if (lambda (entry)  (every (lambda (key) (string= (getf entry key) (getf initargs key))) initarg-keys)) complete-index)))
    (if index-entry
        (apply #'make-instance class-name index-entry)
        (format nil "~&File-path: ~A~%complete index: ~A~%keys: ~A" file-path complete-index initarg-keys))))
