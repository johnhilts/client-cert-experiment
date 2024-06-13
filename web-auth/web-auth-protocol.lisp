;;;; protocol for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defclass web-auth-pages () ;; TODO this is a weird name ... let's change it
  ((%on-auth-hook
    :reader on-auth-hook
    :initarg :on-auth-hook)
   (%show-auth-failure
    :reader show-auth-failure
    :initarg :show-auth-failure
    :initform (error "Registering a function for show-auth-failure is required."))
   (%find-user-info
    :reader find-user-info
    :initarg :find-user-info
    :initform (error "Registering a function for find-user-info is required."))
   (%find-secure-user-info
    :reader find-secure-user-info
    :initarg :find-secure-user-info
    :initform (error "Registering a function for find-secure-user-info is required.")) 
   (%login-page
    :reader login-page
    :initarg :login-page
    :initform  (error "Registering a function for login-page is required."))        
   (%signup-page
    :reader signup-page
    :initarg :signup-page
    :initform  (error "Registering a function for signup-page is required."))
   (%session-user-map
    :reader session-user-map
    :initform (make-hash-table))))

(defgeneric use-web-auth (web-auth-pages)
  (:documentation "Input: web-auth-pages. Use this to enable user auth in a web app."))

(defgeneric establish-user-session (application-user)
  (:documentation "Establish the user session in Hunchentoot's session apparatus + in cookies.
This probably needs some re-working but is serviceable for now."))
