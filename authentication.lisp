(in-package #:cl-vote)

(defun auth-params (&rest k/v-pairs)
  (let ((params (loop for (k v) on k/v-pairs by #'cddr
		   collect (cons k v))))
    (if *dev-mode*
	(cons
	 (cons "redirect_uri" (format nil "http://localhost:~a/auth/github/callback" *http-port*))
	 params)
	params)))

(define-handler (auth/github/callback :content-type "text/plain") ((code :string))
  (let* ((raw (uiop:run-program
	       (format nil "curl -X POST -d 'client_id=~a&client_secret=~a&code=~a' https://github.com/login/oauth/access_token"
		       +github-api-id+ +github-api-secret+ code)
	       :output :string))
	 (params (house::parse-params :form-encoded raw)))
    (aif (cdr (assoc :access_token params))
	 (let* ((raw (uiop:run-program
		      (format nil "curl https://api.github.com/user?access_token=~a" it)
		      :output :string))
		(u (yason:parse raw :object-key-fn #'house::->keyword)))
	   (format t "GOT USER!~%  ~s~%  ~s ~s~%" (alexandria:hash-table-alist u) (gethash :login u) (gethash :html_url u))
	   (setf (lookup :user session)
		 (make-instance
		  'user
		  :source :github :access-token it
		  :name (gethash :login u) :url (gethash :html_url u)))
	   (format t "SET USER IN SESSION~%")
	   (format t "TOKEN: ~a~%" (house::token session))
	   (format t "REDIRECTING...~%")
	   (redirect! "/"))
	 "AUTHENTICATION ERROR")))

(defmacro logged-in-only (&body body)
  `(cond ((lookup :user session)
	  ,@body)
	 (t (redirect! (format nil "https://github.com/login/oauth/authorize?client_id=~a" +github-api-id+)))))
