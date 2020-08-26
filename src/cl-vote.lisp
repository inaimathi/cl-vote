;;;; src/cl-vote.lisp
(in-package #:cl-vote)
(named-readtables:in-readtable clj:syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; handlers
(defmacro page-template (&body contents)
  `(who:with-html-output-to-string (html)
     (:html
      (:head)
      (:body
       (:div :id "contents"
	     ,@contents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; user profiles
(hunchentoot:define-easy-handler (user/qrcode :uri "/user/qrcode") ()
  (if-let (user (hunchentoot:session-value :user))
    (progn (setf (hunchentoot:content-type*) "image/png")
	   (user-qrcode user))
    (progn (setf (hunchentoot:return-code*) 400)
	   "No qrcode found")))

(hunchentoot:define-easy-handler (user/profile :uri "/user/profile") ()
  (if-let (user (hunchentoot:session-value :user))
    (page-template
      (:p "Hello, " (:code (cl-who:str (lookup user :name))) "!")
      ;; (:p "Your recovery token is "
      ;; 	  (:code (cl-who:str (lookup user :recovery-token))) " Please write it down in case you lose access to your authenticator app.")
      (:img :src "/user/qrcode"))
    (hunchentoot:redirect "/auth/login")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; login and registration
(hunchentoot:define-easy-handler (action/logout :uri "/action/logout") ()
  (hunchentoot:remove-session (hunchentoot:start-session))
  (hunchentoot:redirect "/"))

(hunchentoot:define-easy-handler (action/register :uri "/action/register") (name)
  (if-let (user (user-by-name name))
    (hunchentoot:redirect "/auth/register")
    (progn (register-user! name)
	   (hunchentoot:start-session)
	   (setf (hunchentoot:session-value :user) (user-by-name name))
	   (hunchentoot:redirect "/user/profile"))))

(hunchentoot:define-easy-handler (auth/register :uri "/auth/register") ()
  (page-template
    (:form :action "/action/register" :method :post
	   (:input :type "text" :name "name" :placeholder "User Name")
	   (:input :type "submit" :value "Register"))))

(hunchentoot:define-easy-handler (action/login :uri "/action/login") (name token next)
  (let ((user (user-by-name name)))
    (if (and user (user-authorized? user token))
	(progn (hunchentoot:start-session)
	       (setf (hunchentoot:session-value :user) user)
	       (hunchentoot:redirect (or next "/")))
	(hunchentoot:redirect "/auth/login"))))

(hunchentoot:define-easy-handler (auth/login :uri "/auth/login") ()
  (page-template
    (:form
     :action "/action/login" :method :post
     (:input :type "text" :name "name" :placeholder "User Name")
     (:input :type "text" :name "token" :placeholder "Auth Token")
     (:input :type "submit" :value "Login"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; elections
(hunchentoot:define-easy-handler (action/election/create :uri "/action/election/create") (election)
  :todo)
(hunchentoot:define-easy-handler (election/create :uri "/election/create") ()
  :todo)

(hunchentoot:define-easy-handler (action/election/vote :uri "/action/election/vote") (ballot)
  :todo)
(hunchentoot:define-easy-handler (election/vote :uri "/election/vote") (election-id)
  :todo)

(hunchentoot:define-easy-handler (election :uri "/election") (election-id)
  :todo)
(hunchentoot:define-easy-handler (election/manage :uri "/election/manage") (election-id) :todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; main page
(hunchentoot:define-easy-handler (root :uri "/") ()
  (page-template
    (if-let (user (hunchentoot:session-value :user))
      (cl-who:htm (:p "You are logged in as " (:b (cl-who:str (lookup user :name))))
		  (:ul
		   (:li (:a :href "/action/logout" "Logout"))))
      (cl-who:htm (:p "You are not logged in")
		  (:ul
		   (:li (:a :href "/auth/login" "Login"))
		   (:li (:a :href "/auth/register" "Register")))))))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 5555))
