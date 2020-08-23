;;;; src/cl-vote.lisp
(in-package #:cl-vote)
(named-readtables:in-readtable clj:syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; model
(defparameter *base* (fact-base:base! #P "cl-vote.base"))
(ensure-directories-exist "qrcodes")

(defun user-by-name (user-name)
  (first
   (fact-base:for-all
    `(and (?id :user t) (?id :name ,user-name) (?id :secret ?secret))
    :in *base* :collect {:id ?id :name user-name :secret ?secret})))

(defun register-user! (user-name)
  (fact-base:multi-insert!
   *base* `((:user t) (:name ,user-name) (:secret ,(mk-otp-secret)))))


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
	   (:input :type "text" :name "name")
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
    (:form :action "/action/login" :method :post
	   (:input :type "text" :name "name")
	   (:input :type "text" :name "token")
	   (:input :type "submit" :value "Login"))))

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
