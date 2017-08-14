(in-package #:cl-vote)
;;; "cl-vote" goes here. Hacks and glory await!

(define-handler (/) ()
  (let ((u (lookup :user session)))
    (with-html-output-to-string (s nil :prologue t :indent t)
      (flet ((papers-list (papers)
	       (htm (:ul (loop for p in papers
			    for href = (gethash :link p)
			    for date = (universal->string (gethash :date p))
			    do (htm
				(:li :paperId (gethash :id p)
				     :paperTitle (gethash :title p)
				     :paperDescription (gethash :body p)
				     :paperDate date
				     (if href
					 (htm (:a :href href (str (gethash :title p))))
					 (htm (str (gethash :title p))))
				     (when date (htm " - " (:span :class "date" (str date)))))))))))
	(htm (:html (:head
		     (:title "Papers")
		     (:link :rel "stylesheet" :href "/css/main.css")
		     (:script :type "text/javascript" :src "/js/base.js")
		     (:script :type "text/javascript" :src "/js/main.js")
		     (:script
		      :type "text/javascript"
		      (str (ps* `(defvar +user+ ,(when u `(create :source ,(source u) :name ,(name u) :url ,(url u))))))))
		    (:body
		     (:div
		      :class "header"
		      (if u
			  (htm
			   (:a :href "/logout" "Logout") " - "
			   (:span "Hello there, " (:a :href (url u) (str (name u)))))
			  (htm (:a :href "/login" "Login with Github"))))
		     (:div
		      :class "papers-panel schedule"
		      (:h3 "Reading Schedule")
		      (papers-list (get-scheduled-papers)))
		     (:div
		      :class "papers-panel history"
		      (:h3 "Past Papers")
		      (papers-list (get-past-papers)))
		     (:div
		      :class "papers-panel future"
		      (:h3 "Future Papers")
		      (papers-list (get-future-papers))))))))))

(define-handler (login) ()
  (logged-in-only
    (redirect! "/")))

(define-handler (logout) ()
  (setf (lookup :user session) nil)
  (redirect! "/"))

(defun start! (port &key (host usocket:*wildcard-host*))
  (let ((s *standard-output*))
    (setf *http-port* port
	  *thread*
	  (bt:make-thread
	   (lambda ()
	     (format s "Listening on ~{~a~^.~}:~a...~%" (coerce host 'list) port)
	     (house:start port host))
	   :name "papers-http-thread"))
    nil))

(defun stop! ()
  (bt:destroy-thread *thread*)
  (bt:destroy-thread *thread*)
  (setf *thread* nil))
