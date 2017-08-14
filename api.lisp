(in-package #:cl-vote)

(define-json-handler (api/vote :method :post) ()
  (when (and (>= (votes-remaining (lookup :user session))
		 (length parameters)))
    (loop for (k . v) in (parameters request)
       collect (cons (parse-integer (symbol-name k)) 1))))

(define-json-handler (api/user/-user-name=string/votes) ()
  (cons (cons :remaining (votes-remaining user-name))
	(get-active-votes-for user-name)))

(define-handler (api/paper :method :put :content-type "application/json") ()
  (list :todo "Submit a new paper for voting"))

(define-handler (api/paper :method :delete :content-type "application/json") ()
  (list :todo "Withdraw a paper from being voted on"))

(define-json-handler (api/paper :method :get) ()
  (list :todo "Get the listing of { past, scheduled, future } papers. Each slot is [{title, link, description, submitter, votes}]"))
