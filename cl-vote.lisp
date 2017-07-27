(in-package #:cl-vote)
;;; "cl-vote" goes here. Hacks and glory await!

(define-handler (/) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (flet ((papers-list (papers)
	     (htm (:ul (loop for p in papers
			  for href = (gethash :link p)
			  if href do (htm (:li (:a :href href (str (gethash :title p)))))
			  else do (htm (:li (str (gethash :title p)))))))))
      (htm
       (:html
	(:head (:title "Vote"))
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

(define-handler (api/vote :method :post :content-type "application/json") ()
  (list :todo "Submit a vote slate"))

(define-handler (api/paper :method :put :content-type "application/json") ()
  (list :todo "Submit a new paper for voting"))

(define-handler (api/paper :method :delete :content-type "application/json") ()
  (list :todo "Withdraw a paper from being voted on"))
