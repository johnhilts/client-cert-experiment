;;;; functions to handle auth related concerns. Depends on hunchentoot.
(cl:in-package #:jfh-web-auth)

(tbnl:define-easy-handler (authenticate-handler :uri "/auth") (user-login password redirect-back-to)
  (let ((user-info (funcall (find-secure-user-info *web-auth-pages*) user-login)))
    (if
     (and user-info
          (string=
           (jfh-app-core:user-password user-info)
           (jfh-utility:hash-password password)))
     (progn
       (establish-user-session user-info)
       (funcall (on-auth-hook *web-auth-pages*))
       (tbnl:redirect redirect-back-to))
     (funcall (show-auth-failure *web-auth-pages*)))))

(tbnl:define-easy-handler (login-page-handler :uri "/login") (redirect-back-to)
  (funcall (login-page *web-auth-pages*) redirect-back-to))

(tbnl:define-easy-handler (signup-page-handler :uri "/signup") ()
  (funcall (signup-page *web-auth-pages*)))

(tbnl:define-easy-handler (logout-page-handler :uri "/logout") ()
  "logout endpoint"
  (format t
	  "~&www-authorization: ~A, authorization: *** ~A ***~%"
	  (tbnl:header-out :www-authenticate)
	  (tbnl:header-out "authorization"))
  (remhash (tbnl:session-value 'the-session) (session-user-map *web-auth-pages*))
  (tbnl:delete-session-value 'the-session)
  (setf (tbnl:header-out :www-authenticate) nil)
  (tbnl:redirect "/login"))
