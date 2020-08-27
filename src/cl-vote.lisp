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

(defmacro define-easy-handler (name args &body body)
  `(hunchentoot:define-easy-handler (,name :uri ,(if (eq name 'root) "/" (format nil "/~(~a~)" name))) ,args
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; user profiles
(define-easy-handler user/qrcode ()
  (if-let (user (hunchentoot:session-value :user))
    (progn (setf (hunchentoot:content-type*) "image/png")
	   (user-qrcode user))
    (progn (setf (hunchentoot:return-code*) 400)
	   "No qrcode found")))

(define-easy-handler user/recovery-token ()
  (if-let (user (hunchentoot:session-value :user))
    (let ((token (mk-recovery-token)))
      (fresh-recovery-token! (lookup user :id) token)
      (page-template
	(:p "Your new recovery token is")
	(:pre (cl-who:str token))
	(:p "Write it down in case you lose access to your authenticator.")
	(:p (:a :href "/user/profile" "Ok"))))
    (hunchentoot:redirect "/auth/login")))

(define-easy-handler user/profile ()
  (if-let (user (hunchentoot:session-value :user))
    (page-template
      (:p "Hello, " (:code (cl-who:str (lookup user :name))) "!")
      (:p (:a :href "/user/recovery-token" "Get a recovery token")
	  (:i " (This will expire your previous recovery token)"))
      (:img :src "/user/qrcode"))
    (hunchentoot:redirect "/auth/login")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; login and registration
(define-easy-handler action/logout ()
  (hunchentoot:remove-session (hunchentoot:start-session))
  (hunchentoot:redirect "/"))

(define-easy-handler action/register (name)
  (if-let (user (user-by-name name))
    (hunchentoot:redirect "/auth/register")
    (progn (register-user! name)
	   (hunchentoot:start-session)
	   (setf (hunchentoot:session-value :user) (user-by-name name))
	   (hunchentoot:redirect "/user/profile"))))

(define-easy-handler auth/register ()
  (page-template
    (:form :action "/action/register" :method :post
	   (:input :type "text" :name "name" :placeholder "User Name")
	   (:input :type "submit" :value "Register"))))

(define-easy-handler action/login (name token next)
  (let ((user (user-by-name name)))
    (cond ((and user (user-authorized? user token))
	   (hunchentoot:start-session)
	   (setf (hunchentoot:session-value :user) user)
	   (hunchentoot:redirect (or next "/")))
	  ((and user (tomb:tomb-matches? token (lookup user :recovery-token)))
	   (hunchentoot:start-session)
	   (setf (hunchentoot:session-value :user) user)
	   (hunchentoot:redirect "/user/recovery-token"))
	  (t (hunchentoot:redirect "/auth/login")))))

(define-easy-handler auth/login ()
  (page-template
    (:form
     :action "/action/login" :method :post
     (:input :type "text" :name "name" :placeholder "User Name")
     (:input :type "text" :name "token" :placeholder "Auth Token")
     (:input :type "submit" :value "Login"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; elections
(define-easy-handler action/election/create (election)
  :todo)
(define-easy-handler election/create ()
  :todo)

(define-easy-handler action/election/vote (ballot)
  :todo)
(define-easy-handler election/vote (election-id)
  :todo)

(define-easy-handler election (election-id)
  :todo)
(define-easy-handler election/manage (election-id) :todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; main page
(define-easy-handler root ()
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
