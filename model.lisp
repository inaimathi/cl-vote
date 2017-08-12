(in-package #:cl-vote)

(defparameter +vote-total+ 5)

(defparameter *public-data*
  (fact-base:base!
   (merge-pathnames "cl-vote.base" (user-homedir-pathname))
   :in-memory? t))

(defun hash (&rest k/v-pairs)
  (let ((h (make-hash-table)))
    (loop for (k v) on k/v-pairs by #'cddr
       do (setf (gethash k h) v))
    h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Users
(defclass user ()
  ((source :reader source :initform :github :initarg :source)
   (name :accessor name :initarg :name)
   (access-token :accessor access-token :initarg :access-token)
   (url :reader url :initarg :url)))

(defmethod qualified-name ((u user))
  (format nil "~(~a~):~a" (source u) (name u)))

(defmethod get-or-create-user-id ((name string))
  (or (first
       (fact-base:for-all
	`(?id :user ,name)
	:in *public-data* :collect ?id))
      (fact-base:insert-new!
       *public-data* :user name)))
(defmethod get-or-create-user-id ((u user))
  (get-or-create-user-id (qualified-name u)))

(defmethod active-votes ((qualified-user-name string))
  (let ((user-id (get-or-create-user-id qualified-user-name)))
    (fact-base:for-all
     `(and (?id :cast-by ,user-id)
	   (not (?id :counted nil))
	   (?id :vote ?paper-id)
	   (?id :vote-score ?score))
     :in *public-data*
     :collect (list ?paper-id qualified-user-name ?score))))
(defmethod active-votes ((u user))
  (active-votes (qualified-name u)))

(defmethod votes-remaining (fixme)
  ;; Delete this once the user system is operational
  +vote-total+)
(defmethod votes-remaining ((u user))
  (let ((scores (mapcar #'third (active-votes u))))
    (apply #'- +vote-total+ (or scores '(0)))))

(defmethod user-href ((qualified-user-name string))
  (destructuring-bind (site name) (cl-ppcre:split ":" qualified-user-name :limit 2)
    (let ((template (case (house::->keyword site)
		      (:github "https://github.com/~a")
		      (t nil))))
      (with-html-output (*standard-output*)
	(if template
	    (htm (:a :href (format nil template name) :target "_BLANK" (str name)))
	    (str name))))))
(defmethod user-href ((u user))
  (user-href (qualified-name u)))

(defun fetch-all-issues-from-github ()
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

(defun submit-papers-from-issues! ()
  (loop for (user title link body) in (fetch-all-issues-from-github)
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

(defun register-vote! (qualified-user-name paper-id &key (score 1))
  (let ((user-id (get-or-create-user-id qualified-user-name)))
    (fact-base:multi-insert!
     *public-data*
     `((:vote ,paper-id)
       (:cast-by ,user-id)
       (:vote-score ,score)))
    nil))

(defun submit-paper! (qualified-user-name paper-title paper-link)
  (let ((paper-id
	 (fact-base:multi-insert!
	  *public-data*
	  `((:paper nil)
	    (:title ,paper-title)
	    (:link ,paper-link))))
	(user-id (get-or-create-user-id qualified-user-name)))
    (fact-base:insert! *public-data* (list user-id :submitted paper-id))
    (list :id paper-id :title paper-title :link paper-link)))

(defun schedule-paper! (qualified-user-name paper-id scheduled-day)
  (unless (fact-base:for-all `(,paper-id :scheduled ?) :in *public-data* :collect t)
    (let ((u (local-time:timestamp-to-universal scheduled-day)))
      (fact-base:insert! *public-data* (list paper-id :scheduled-by qualified-user-name))
      (fact-base:insert! *public-data* (list paper-id :scheduled u))
      (fact-base:for-all
       `(?id :vote ,paper-id) :in *public-data*
       :do (fact-base:insert!
	    *public-data* (list ?id :counted nil))))))

(defun complete-paper! (qualified-user-name paper-id)
  (unless (fact-base:for-all `(,paper-id :read ?) :in *public-data* :collect t)
    (let ((u (local-time:timestamp-to-universal (local-time:now))))
      (fact-base:insert! *public-data* (list paper-id :marked-read-by qualified-user-name))
      (fact-base:insert! *public-data* (list paper-id :read u)))))

(defun get-scheduled-papers ()
  (fact-base:for-all
   (and (?id :paper nil) (?id :title ?title) (?id :link ?link)
	(?id :scheduled ?date) (not (?id :read ?)))
   :in *public-data*
   :collect (hash :title (string-trim "\"'\"" ?title) :link ?link :id ?id :date ?date)))

(defun get-past-papers ()
  (fact-base:for-all
   (and (?id :paper nil) (?id :title ?title) (?id :link ?link)
	(?id :read ?date))
   :in *public-data*
   :collect (hash :title (string-trim "\"'\"" ?title) :link ?link :id ?id :date ?date)))

(defun get-future-papers ()
  (fact-base:for-all
   (and (?id :paper nil)
	(not (?id :read ?)) (not (?id :scheduled ?))
	(?id :title ?title) (?id :link ?link))
   :in *public-data*
   :collect (hash :title (string-trim "\"'\"" ?title) :link ?link :id ?id)))
