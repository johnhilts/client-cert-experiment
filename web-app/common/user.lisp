;;;; functions to support web app users.
(cl:in-package #:jfh-certs-web-app)

(defmethod print-object ((web-app-user web-app-user) stream)
  "Print web app user."
  (call-next-method)
  (print-unreadable-object (web-app-user stream :type t)
    (with-accessors ((user-name user-name)) web-app-user
      (format stream
       	      "User Name: ~A" user-name))))

(defun add-user (user-name user-login user-password)
  "Add new user."
  (jfh-app-core:save-new-application-user (make-web-app-user user-name user-login user-password) (jfh-app-core:application-configuration *web-configuration*)))

(defun make-web-app-user (user-name user-login user-password &optional (user-id ""))
  "Constructor for web-app-user."
  (make-instance 'web-app-user :user-name user-name :user-login user-login :user-password user-password :user-id user-id))

(defmethod jfh-app-core:save-application-user ((web-app-user web-app-user) (application-configuration jfh-app-core:application-configuration))
  "Input: web-app-user and app-configuration. Output: serialized web-app-user (sub-class specific fields only) . Persist application user info."
  (call-next-method)
  (let ((file-name "web-app-user.sexp")
        (user-info-list (list
                         :user-name (user-name web-app-user))))
    (jfh-app-core:save-user file-name user-info-list web-app-user application-configuration)))

(defun find-web-user-info (user-login)
  "Derive web-user info from app-user."
  (let* ((application-user (jfh-app-core:find-user-info user-login))
         (user-id (jfh-app-core:user-id application-user))
         (web-user-info (jfh-app-core:read-user-info user-id "web-app-user.sexp"))
	 (secure-user-info (jfh-app-core:read-user-info user-id "hash.sexp"))) ;; TODO make the file name an exported string
    (make-web-app-user (getf web-user-info :user-name) user-login (getf secure-user-info :user-password) user-id)))
