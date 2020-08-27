;;;; cl-vote.asd

(asdf:defsystem #:cl-vote
  :description "A tool for collective decision making"
  :author "inaimathi<leo.zovic@google.com>"
  :license "GPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:clj
	       #:tomb #:session-token #:cl-base32 #:cl-one-time-passwords #:cl-qrencode #:ironclad
	       #:cl-who #:parenscript #:cl-css
	       #:fact-base #:hunchentoot)
  :components ((:module
		src :components
		((:file "package")
		 (:file "authentication")
		 (:file "model")
		 (:file "cl-vote")))))

(asdf:defsystem #:cl-vote-test
  :description "Test suite for :cl-vote"
  :author "inaimathi<leo.zovic@google.com>"
  :license "GPL3"
  :serial t
  :depends-on (#:cl-vote #:test-utils)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module
                test :components
                ((:file "package")
                 (:test-file "cl-vote"))))
  :perform (test-op
	    :after (op c)
	    (funcall (intern #.(string :run) :prove) c)))
