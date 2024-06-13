;;;; functions for auth related to this web app; usually called from a page handler
(cl:in-package #:jfh-certs-web-app)

(defun show-auth-failure ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Auth Failure"))
     (:body
      (:h2 "Authorization failed!")
      (:div "User or password didn't match"
            (:a :href "/login" "Click here to try again!"))))))

(defun login-page (redirect-back-to)
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Login"))
     (:body
      (:h2 "Use this page to Login!")
      (:form :method "post" :action "auth"
             (:input :type "hidden" :name "redirect-back-to" :value (or redirect-back-to "/daily-tip"))
             (:div :id "login-input-div"
              (:div (:input :name "user-login" :type "email" :placeholder "Login" :class "login-input" :autofocus "autofocus"))
              (:div (:input :name "password" :type "password" :placeholder "Password" :class "login-input"))
              (:div (:button "Login") (:span "&nbsp;") (:button :id "sign-up-button" :type "button" :onclick "javascript:location.href=\"/signup\";" "Sign-Up"))))))))

(defun signup-page ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Signup"))
     (:body
      (if (or
           (tbnl:post-parameter "name")
           (tbnl:post-parameter "user")
           (tbnl:post-parameter "password")
           (tbnl:post-parameter "confirm-password"))
          (multiple-value-bind (signup-validation-successful signup-validation-failure-reasons)
              (auth:validate-signup-parameters (tbnl:post-parameter "name") (tbnl:post-parameter "user") (tbnl:post-parameter "password") (tbnl:post-parameter "confirm-password"))
            (if signup-validation-successful
                (progn
                  (add-user (tbnl:post-parameter "name") (tbnl:post-parameter "user") (tbnl:post-parameter "password"))
                  (let ((user-info (jfh-app-core:find-user-info (tbnl:post-parameter "user"))))
                    (auth:establish-user-session user-info))
                  (who:htm (:script :type "text/javascript"
                                    (who:str
                                     (ps:ps
                                      (alert "Signup Successful!")
                                      (setf (ps:@ location href) "/daily-tip"))))))
                (who:htm
                 (:div
                  (:span (who:fmt "Signup Failed, because <ul>~{<li>~a</li>~% ~}</ul>" signup-validation-failure-reasons)))
                 (:div
                  (:span "Please try again: ")
                  (:p (:a :href "/signup" "Back to Signup"))
                  (:p (:a :href "/login" "Back to Login"))))))
          (who:htm
           (:h2 "Use this page to sign-up!")
           (:div
            (:a :href "/login" "Back to Login"))
           (:form :method "post" :action "/signup"
                  (:div
                   (:div (:input :name "name" :type "text" :placeholder "Your Name" :class "login-input" :autofocus "autofocus"))
                   (:div (:input :name "user" :type "email" :placeholder "Login" :class "login-input"))
                   (:div (:input :name "password" :type "password" :placeholder "Password" :class "login-input"))
                   (:div (:input :name "confirm-password" :type "password" :placeholder "Confirm Password" :class "login-input"))
                   (:div (:button "Submit"))))))))))

(defun on-auth-hook ()
  "Run this when authorization is successful."
  (multiple-value-bind (authenticated-user-id present-p)
      (auth:get-authenticated-user)
    (when present-p
      (when (eq (gethash authenticated-user-id *notes* #1='#:empty) #1#)
        (let ((notes-path (jfh-certs-web-app::get-notes-path authenticated-user-id)))
          (read-user-notes authenticated-user-id notes-path)))))
  '#:auth-hook)
