;;;; cl-vote.asd

(asdf:defsystem #:cl-vote
  :description "Describe cl-vote here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:fact-base
               #:house
               #:alexandria
	       #:anaphora
               #:yason
               #:cl-who
               #:cl-css
               #:parenscript
               #:local-time
               #:cl-ppcre
               #:split-sequence)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "secrets")
               (:file "model")
	       (:file "authentication")
	       (:file "api") (:file "css") (:file "js")
	       (:file "cl-vote")))
