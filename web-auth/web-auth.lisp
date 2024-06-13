;;;; functions for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defparameter *web-auth-pages* nil)

(defmethod use-web-auth ((web-auth-pages web-auth-pages))
  "Input: web-auth-pages. Use this to enable user auth in a web app."
  (setf *web-auth-pages* web-auth-pages))

(defun get-authenticated-user ()
  "Get the authenticated user from server session."
  (gethash (tbnl:session-value 'the-session) (session-user-map *web-auth-pages*)))

(defmethod establish-user-session ((application-user jfh-app-core:application-user))
  "Establish the user session in Hunchentoot's session apparatus + in cookies.
This probably needs some re-working but is serviceable for now."
  (let ((session-token (jfh-utility:generate-unique-token)))
    (setf (tbnl:session-value 'the-session) session-token)
    (tbnl:set-cookie (string 'the-session) :value session-token :secure t :http-only t)
    (setf (gethash session-token (session-user-map *web-auth-pages*)) (jfh-app-core:user-id application-user))))

(defun validate-signup-parameters (name user-login password confirm-password)
  "Validate the values used to signup a user."
  (flet ((exists (user-login)
           (funcall (find-user-info *web-auth-pages*) user-login)))
    (let ((signup-validation-failure-reasons ()))
      (if (or
	   (zerop (length name))
	   (zerop (length user-login))
	   (zerop (length password))
	   (zerop (length confirm-password)))
          (push "Please enter all fields." signup-validation-failure-reasons))
      (progn
        (when (exists user-login)
          (push "User already exists; please login." signup-validation-failure-reasons))
        (when (not (string= password confirm-password))
          (push "Passwords don't match." signup-validation-failure-reasons)))
      (values
       (zerop (length signup-validation-failure-reasons))
       signup-validation-failure-reasons))))
