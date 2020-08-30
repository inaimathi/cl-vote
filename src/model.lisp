;;;; src/model.lisp
(in-package #:cl-vote)
(named-readtables:in-readtable clj:syntax)

(defparameter *base* (fact-base:base! #P "cl-vote.base"))
(ensure-directories-exist "qrcodes")

(defun user-by-name (user-name)
  (first
   (fact-base:for-all
       `(and (?id :user t) (?id :name ,user-name) (?id :secret ?secret) (?id :recovery-token ?token))
    :in *base* :collect {:id ?id :name user-name :secret ?secret :recovery-token ?token})))

(defun list-users ()
  (fact-base:for-all
      (and (?id :user t) (?id :name ?user-name))
    :in *base* :collect {:id ?id :name ?user-name}))

(defun fresh-recovery-token! (user-id token)
  (fact-base:for-all
      `(,user-id :recovery-token ?token)
    :in *base* :do (fact-base:delete! *base* (list user-id :recovery-token ?token)))
  (fact-base:insert!
   *base* (list user-id :recovery-token (tomb:entomb token))))

(defun register-user! (user-name)
  (fact-base:multi-insert!
   *base* `((:user t)
	    (:name ,user-name)
	    (:recovery-token nil)
	    (:secret ,(mk-otp-secret)))))

(defun list-elections ()
  (fact-base:for-all
      (and (?id :election t) (?id :name ?user-name))
    :in *base* :collect {:id ?id :name }))

(defun election-by-id (id)
  (let ((candidates (fact-base:for-all
			`(and (?id :in-election ,id)
			      (?id :candidate t)
			      (?id :name ?name))
		      :in *base* :collect {:id ?id :name ?name})))
    (first
     (fact-base:for-all
	 `(and (,id :election t)
	       (,id :decision-time ?decision)
	       (,id :ballot-type ?ballot)
	       (,id :title ?title))
       :in *base* :collect {:id id :title ?title
				:ballot-type ?ballot
				:decision-time ?decision
				:candidates candidates}))))

(defun create-election! (creator-id title &key candidates (ballot-type '(:range 0 5)) (decision-time :open))

  (assert (or (and (== :range (first ballot-type))
		   (destructuring-bind (_ a b) ballot-type
		     (declare (ignore _))
		     (and (integerp a) (integerp b)
			  (> b a))))
	      (and (== :points (first ballot-type))
		   (integerp (second ballot-type))
		   (>= 1 (second ballot-type)))))

  (assert (or (== :open decision-time)
	      (== :all-voted decision-time)
	      (== :date (first decision-time))
	      (== :vote-count (first decision-time))))

  (let ((id (fact-base:multi-insert!
	     *base* `((:election t)
		      (:creator ,creator-id)
		      (:title ,title)
		      (:ballot-type ,ballot-type)
		      (:decision-time ,decision-time)))))
    (loop for c in candidates
       do (fact-base:multi-insert!
	   *base* `((:candidate t)
		    (:name ,c)
		    (:in-election ,id))))
    id))
