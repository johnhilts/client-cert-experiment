;;;; Web pages for certs
(cl:in-package #:jfh-certs-web-app)

(auth:define-protected-page (daily-tip "/daily-tip") ()
  "daily tip page"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Daily Tip from your certs"))
     (:body
      (:div
       (who:str "placeholder text hyah"))))))

(auth:define-protected-page (daily-tip-simple "/daily-tip-simple") ()
  "daily tip page (simple string only version no markup)"
  "Placeholder text hyahh")

(auth:define-protected-page (search-handler "/search") ()
  "search page"
  (let((title-id-prefix "title"))
    (flet ((get-title-checked-from-request ()
             (remove-if-not
              (lambda (e)
                (jfh-utility:string-starts-with title-id-prefix (car e)))
              (tbnl:post-parameters tbnl:*request*))))
      (let ((query (tbnl:post-parameter "query")))
        (who:with-html-output-to-string
	    (*standard-output* nil :prologue t :indent t)
          (:html
           (who:str (common-header "Search your certs"))
           (:body
	    (:div
	     (:form :method "post" :action "search"
		    (:div
		     (:div (:textarea :id "query" :name "query" :placeholder "Write text to search on here" :autofocus "autofocus" (who:str (if query query ""))))
		     (:div (:button "Search"))
		     (when query
		       (let* ((in (mapcar 'cdr (get-title-checked-from-request)))
			      (results (list "result one")))
		         (when results
		           (who:htm
			    (:span (who:str results)))))))
		    (let ((titles (list "title one"))
		          (checked (mapcar 'car (get-title-checked-from-request))))
		      (who:htm
		       (:div
		        (loop for title in titles
			      for i = 1 then (incf i)
			      for title-id = (format nil "~A~D" title-id-prefix i)
			      do
			         (who:htm
			          (:div
			           (who:htm
                                    (:input :type "checkbox" :id title-id :name title-id :value (who:str title)
                                            :checked (if (find title-id checked :test #'string=) t nil)))
			           (:label :for title-id (who:str title)))))))))))))))))

(auth:define-protected-page (upload-list-handler "/upload-list") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Daily Tip from your certs"))
     (:body
      (:div
       (who:str (upload-list auth:authenticated-user)))))))

(auth:define-protected-page (uploaded-titles-handler "/uploaded-titles") ()
  (uploaded-titles auth:authenticated-user))

(auth:define-protected-page (upload-handler "/upload") ()
  (upload auth:authenticated-user))
