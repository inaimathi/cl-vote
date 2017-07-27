(in-package #:cl-vote)
;;; "cl-vote" goes here. Hacks and glory await!

(define-handler (/) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (flet ((papers-list (papers)
	     (htm (:ul (loop for p in papers
			  for href = (gethash :link p)
			  if href do (htm (:li (:a :href href (str (gethash :title p)))))
			  else do (htm (:li (str (gethash :title p)))))))))
      (htm (:html (:head (:title "home - Papers"))
		  (:body
		   (:div
		    (:h1 "Reading Schedule")
		    (papers-list (get-scheduled-papers)))
		   (:div
		    (:h3 "Future Papers")
		    (papers-list (get-future-papers)))
		   (:div
		    (:h3 "Past Papers")
		    (papers-list (get-past-papers)))))))))

(define-handler (ballot) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head (:title "vote - Papers"))
     (:body
      (:form :action "/api/vote/redirecting" :method "POST"
       (:ul
	(loop for p in (get-future-papers)
	   for href = (gethash :link p)
	   do (htm (:li (:input :type "checkbox" :name (gethash :id p) :value "1")
			(if href
			    (htm (:a :href href :target "_blank"
				     (str (gethash :title p))))
			    (str (gethash :title p)))))))
       (:input :type "submit"))))))

(define-handler (api/vote/redirecting :method :post) ()
  (loop for (k . v) in (parameters request)
     collect (cons (parse-integer (symbol-name k)) 1))
  (redirect! "/"))

(define-json-handler (api/vote :method :post) ()
  (loop for (k . v) in (parameters request)
     collect (cons (parse-integer (symbol-name k)) 1)))

(define-handler (api/paper :method :put :content-type "application/json") ()
  (list :todo "Submit a new paper for voting"))

(define-handler (api/paper :method :delete :content-type "application/json") ()
  (list :todo "Withdraw a paper from being voted on"))
