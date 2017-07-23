(in-package #:cl-vote)
;;; "cl-vote" goes here. Hacks and glory await!

(define-handler (/) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head (:title "Vote"))
     (:body (:p (:b "TODO - ")
		"List existing papers in order of total upvotes by caballers, already read papers and the current reading schedule.")))))

(define-handler (api/vote :method :post :content-type "application/json") ()
  (list :todo "Submit a vote slate"))

(define-handler (api/paper :method :put :content-type "application/json") ()
  (list :todo "Submit a new paper for voting"))

(define-handler (api/paper :method :delete :content-type "application/json") ()
  (list :todo "Withdraw a paper from being voted on"))
