(in-package #:cl-vote)

(defparameter *public-data*
  (fact-base:base!
   (merge-pathnames "cl-vote.base" (user-homedir-pathname))
   :in-memory? t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Users
(defclass user ()
  ((source :reader source :initform :github :initarg :source)
   (name :accessor name :initarg :name)
   (access-token :accessor access-token :initarg :access-token)
   (url :reader url :initarg :url)))

(defmethod user-id ((u user))
  (format nil "~(~a~):~a" (source u) (name u)))

(defun user-link (qualified-user-name)
  (destructuring-bind (site name) (cl-ppcre:split ":" qualified-user-name :limit 2)
    (let ((template (case (house::->keyword site)
		      (:github "https://github.com/~a")
		      (t nil))))
      (with-html-output (*standard-output*)
	(if template
	    (htm (:a :href (format nil template name) :target "_BLANK" (str name)))
	    (str name))))))

(defun get-all-issues-from-github ()
  (loop for paper in (loop for i from 1
			for pg = (uiop:run-program
				  (format nil "curl https://api.github.com/repos/CompSciCabal/SMRTYPRTY/issues?page=~a" i)
				  :output #'yason:parse)
			while pg when pg append pg)
     collect (list (format nil "github:~a" (gethash "login" (gethash "user" paper)))
		   (gethash "title" paper)
		   (cl-ppcre:scan-to-strings
		    "https?://[^ \\r\\n]+"
		    (gethash "body" paper))
		   (gethash "body" paper))))

(defun get-papers-from-issues ()
  (loop for (user title link body) in (get-all-issues-from-github)
     do (submit-paper! user title link)))

(defun get-voted-papers ()
  (fact-base:for-all
   `(and (?id :paper nil)
	 (?id :title ?title)
	 (?id :link ?uri)
	 (not (?id :read nil))
	 (?uid :submitted ?id)
	 (?uid :caballer nil))
   :in *public-data*
   :collect (list :id ?id :title ?title :link ?uri)))

(defun register-vote! (user-id paper-id &key (score 1))
  (fact-base:insert! *public-data* (list user-id :vote paper-id))
  (fact-base:insert! *public-data* (list user-id :vote-score score))
  nil)

(defun submit-paper! (user-id paper-title paper-link)
  (let ((paper-id
	 (fact-base:multi-insert!
	  *public-data*
	  `((:paper nil)
	    (:title ,paper-title)
	    (:link ,paper-link))))))
  (fact-base:insert!
   *public-data* (list user-id :submitted paper-id))
  (list :id paper-id :title paper-title :link paper-link))
