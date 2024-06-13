;;;; Web pages for certs (uploads)
(cl:in-package #:jfh-certs-web-app)

;;;; this section not used yet ;;;
(defun is-text-file (file)
  (with-open-file (stream file)
      (loop for line = (read-line stream nil)
                while line
                          always (every #'is-printable-ascii line))))

(defun is-printable-ascii (char)
  (let ((char-code (char-code char)))
    (and (>= char-code 32) (<= char-code 126))))

(defun is-text-file-w (file)
  (with-open-file (stream file)
      (loop for line = (read-line stream nil)
                while line
                          always (every #'is-printable-unicode line))))

(defun is-printable-unicode (char)
  (let ((char-code (char-code char)))
    (or (= char-code 10)
        (= char-code 13)
        (= char-code 9)
        (and (>= char-code 32) (<= char-code 126))  ; ASCII
        (and (>= char-code 161) (<= char-code 255)) ; Latin-1 supplement
        (>= char-code 256))))                       ; Other Unicode characters
;;;; this section not used yet (end section) ;;;

(defun get-uploaded-file-index-path (authenticated-user-id)
  "Get the path for the uploaded file index. Input: authenticated-user-id. Output: file path."
  (let* ((configuration (jfh-app-core:application-configuration *web-configuration*))
	 (user-path (jfh-app-core:find-user-path (jfh-app-core:make-application-user authenticated-user-id) configuration)))
    (format nil "~A/uploaded-file-index.sexp" user-path)))

(defun get-uploaded-file-index (authenticated-user-id)
  "Read in the the uploaded files index. Input: User ID. Output: (list new-path new-file-name content-type)"
  (jfh-utility:fetch-or-create-data (get-uploaded-file-index-path authenticated-user-id)))

(defun save-file-upload-index (authenticated-user-id uploaded-file-index-entry) ;; todo make uploaded-file-index-entry a class; this will become a method
  "Input: authenticated-user-id, uploaded file index entry. Output: latest file upload index. Persist user's uploaded file index."
  (let ((uploaded-file-index-path (get-uploaded-file-index-path authenticated-user-id)))
    (flet ((callback (uploaded-file-index)
             (push uploaded-file-index-entry uploaded-file-index)
             (jfh-utility:write-complete-file uploaded-file-index-path
					      (remove-duplicates uploaded-file-index
								 :test (lambda (e1 e2)
									 (and
									  (string-equal
									   (namestring (car e1))
									   (namestring (car e2)))
									  (string-equal
									   (cadr e1)
									   (cadr e2))))))))
      (ensure-directories-exist uploaded-file-index-path)
      (jfh-utility:fetch-or-create-data uploaded-file-index-path #'callback))))

(defun process-upload-file (post-parameter authenticated-user-id)
  "Handles file uploads."
  (when (and post-parameter
             (listp post-parameter))
    (destructuring-bind (path file-name content-type)
        post-parameter
      ;; strip directory info sent by Windows browsers
      (when (search "Windows" (tbnl:user-agent) :test 'char-equal)
        (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
      (multiple-value-bind
	    (new-path new-file-name)
	  (get-notes-path authenticated-user-id)
	(rename-file path (ensure-directories-exist new-path))
	(read-user-notes authenticated-user-id new-path)
	(let ((uploaded-file-index-entry (list new-path new-file-name content-type)))
	  (save-file-upload-index authenticated-user-id uploaded-file-index-entry)
	  uploaded-file-index-entry)))))

(defun upload (authenticated-user-id)
  "html page to facilitate uploads."
  (let ((file-uploaded nil))
    (when (tbnl:post-parameter "upload-file")
      (setf file-uploaded (process-upload-file (tbnl:post-parameter "upload-file") authenticated-user-id)))
    (tbnl:no-cache)
    (who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (who:str (common-header "Hunchentoot file upload test"))
       (:body
        (:h2 "File Upload Test")
        (:form :method :post :enctype "multipart/form-data"
               (:p "Select a file with your notes"
                   (:p
                    (:input :type :file :name "upload-file")))
               (:p
                (:input :type :submit :value "Upload")))
        (when file-uploaded
	  (who:str
	   (upload-list authenticated-user-id file-uploaded))))))))

(defun upload-list (authenticated-user-id &optional file-uploaded) ;; todo make this a method once uploaded-file-index-entry is a class
  "html page to list and manage uploads."
  (who:with-html-output-to-string (*standard-output* nil :prologue nil)
    (who:htm
     (:p
      (:table :border 1 :cellpadding 5 :cellspacing 0
              (:tr (:td :colspan 3 (:b "Uploaded files")))
	      (let ((uploaded-files (get-uploaded-file-index authenticated-user-id))
		    (new-file-name (if file-uploaded (cadr file-uploaded) "")))
		(loop for (path file-name content-type) in uploaded-files
		      for is-new = (string= new-file-name file-name)
		      do
			 (who:htm
			  (:tr 
			   (:td
			    (when is-new (who:htm (:b "NEW&nbsp;")))
			    (:a :href (format nil "files/~A?path=~A" (tbnl:url-encode file-name) (tbnl:url-encode (namestring path)))
				(who:esc file-name)))
			   (:td (:a :href "/uploaded-titles" "Titles"))
			   (:td :align "right"
				(who:str (ignore-errors
					  (with-open-file (in path)
					    (file-length in))))
				(who:str (format
					  nil "&nbsp;Bytes (type: ~A)"
					  content-type)))))))))
     (:tr (:td :colspan 3 (:b (:a :href "/daily-tip" "See Daily Tip")))))))

(defun uploaded-titles (authenticated-user-id)
  "List the titles from a notes file."
  (let ((titles (list "title one" "title two" "title 3")))
    (who:with-html-output-to-string
	(*standard-output* nil :prologue t :indent t)
      (:html
       (who:str (common-header "Daily Tip from your certs"))
       (:body
	(:div
	 (loop for title in titles
	       do
		  (who:htm
		   (:div (who:str title))))))))))
