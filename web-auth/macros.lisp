;;;; macros for auth related concerns. 
(cl:in-package #:jfh-web-auth)

(defmacro define-protected-page (name-and-end-point params &body body)
  "Macro to DRY pages requiring authentication"
  (let* ((name (car name-and-end-point))
         (end-point (cadr name-and-end-point))
         (possible-description (car body))
         (has-description (and (atom possible-description) (stringp possible-description)))
         (description (if has-description possible-description nil))
         (body-after-description (if has-description (cdr body) body)))
    `(tbnl:define-easy-handler (,name :uri ,end-point) (,@params)
       ,(when description description)
       (multiple-value-bind (authenticated-user present-p)
           (get-authenticated-user)
         (declare (ignorable authenticated-user))
         (if present-p
             ,@body-after-description
             (tbnl:redirect (format nil "/login?redirect-back-to=~a" (tbnl:url-encode ,end-point))))))))
