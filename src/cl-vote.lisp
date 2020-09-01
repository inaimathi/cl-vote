;;;; src/cl-vote.lisp
(in-package #:cl-vote)
(named-readtables:in-readtable clj:syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; handlers
(defmacro partial (&body contents)
  `(cl-who:with-html-output (*standard-output*)
     (cl-who:htm ,@contents)))

(defun sanitized-name (candidate-title)
  (substitute
   #\- #\space
   (string-trim
    " \n" (string-downcase
	   candidate-title))))

(defmacro page-template (&body contents)
  `(who:with-html-output-to-string (*standard-output*)
     (:html
      (:head)
      (:body
       (:div :id "contents"
	     ,@contents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; user profiles
(house:define-handler (user/qrcode :content-type "image/png") ()
  (if-let (user (house:lookup :user house:session))
    (user-qrcode user)
    house::+400+))

(house:define-handler (user/recovery-token) ()
  (if-let (user (house:lookup :user house:session))
    (let ((token (mk-recovery-token)))
      (fresh-recovery-token! (lookup user :id) token)
      (page-template
	(:p "Your new recovery token is")
	(:pre (cl-who:str token))
	(:p "Write it down in case you lose access to your authenticator.")
	(:p (:a :href "/user/profile" "Ok"))))
    (house:redirect! "/auth/login")))

(house:define-handler (user/profile) ()
  (if-let (user (house:lookup :user house:session))
    (page-template
      (:p "Hello, " (:code (cl-who:str (lookup user :name))) "!")
      (:p (:a :href "/user/recovery-token" "Get a recovery token")
	  (:i " (This will expire your previous recovery token)"))
      (:img :src "/user/qrcode"))
    (house:redirect! "/auth/login")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; login and registration
(house:define-handler (action/logout) ()
  (setf (house:lookup :user house:session) nil)
  (house:redirect! "/"))

(house:define-handler (action/register) ((name string))
  (if-let (user (user-by-name name))
    (house:redirect! "/auth/register")
    (progn (register-user! name)
	   (setf (house:lookup :user house:session) (user-by-name name))
	   (house:redirect! "/user/profile"))))

(house:define-handler (auth/register) ()
  (page-template
    (:form
     :action "/action/register" :method :post
     (:ul
      (:li (:input :type "text" :name "name" :placeholder "User Name"))
      (:li (:input :type "submit" :value "Register"))))))

(house:define-handler (action/login) ((name string) (token string))
  (let ((user (user-by-name name)))
    (cond ((and user (user-authorized? user token))
	   (setf (house:lookup :user house:session) user)
	   (house:redirect! "/"))
	  ((and user (tomb:tomb-matches? token (lookup user :recovery-token)))
	   (setf (house:lookup :user house:session) user)
	   (house:redirect! "/user/recovery-token"))
	  (t (house:redirect! "/auth/login")))))

(house:define-handler (auth/login) ()
  (page-template
    (:form
     :action "/action/login" :method :post
     (:ul
      (:li (:input :type "text" :name "name" :placeholder "User Name"))
      (:li (:input :type "text" :name "token" :placeholder "Auth Token"))
      (:li (:input :type "submit" :value "Login"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; elections
(house:define-handler (action/election/create) (title ballot-type min max points candidates)
  (assert (house:lookup :user house:session))
  (assert (contains? #{"points" "range"} ballot-type))
  (let* ((user (house:lookup :user house:session))
	 (id (create-election!
	      (lookup user :id) title
	      :ballot-type
	      (cond ((== ballot-type "points")
		     `(:points ,(parse-integer (or points "0"))))
		    ((== ballot-type "range")
		     `(:range ,(parse-integer (or min "0")) ,(parse-integer (or max "0")))))
	      :candidates
	      (mapcar
	       (lambda (elem) (string-trim (list #\return #\newline #\space #\tab) elem))
	       (split-sequence:split-sequence
		#\newline candidates
		:remove-empty-subseqs t)))))
    (house:redirect! (format nil "/election?election-id=~a" id))))

(house:define-handler (election/create) ()
  (assert (house:lookup :user house:session))
  (page-template
    (cl-who:htm
     (:form
      :action "/action/election/create" :method :post
      (:ul
       (:li (:input :type "text" :name "title" :placeholder "Election Title"))
       (:li (:input :type "radio" :checked "true" :name "ballot-type" :value "points")
	    (:label :for "range" "Points")
	    (:input :type "text" :name "points" :placeholder "Number of Points" :value "1"))
       (:li (:input :type "radio" :name "ballot-type" :value "range")
	    (:label :for "range" "Range")
	    (:input :type "text" :name "min" :placeholder "Min" :value "0")
	    (:input :type "text" :name "max" :placeholder "Max" :value "5"))
       (:li (:textarea :name "candidates" :placeholder "Either comma separated or one-per-line"))
       (:li (:input :type "submit" :value "Create")))))))

(house:define-handler (action/election/vote) ((election-id string) (ballot json))
  (format nil "~a" (house:parameters house:request)))

(defun candidate-input (ballot-type candidate)
  (let* ((b-type (first ballot-type))
	 (min (second ballot-type))
	 (max (third ballot-type))
	 (points (second ballot-type))
	 (name (lookup candidate :name))
	 (safe (sanitized-name name)))
    (cl-who:with-html-output (*standard-output*)
      (cond ((and (== b-type :points) (= 1 points))
	     (cl-who:htm
	      (:li (:input :type "radio" :name "vote" :value safe)
		   (:label :for "vote" (cl-who:str name)))))
	    ((and (== b-type :range) (= min 0) (= max 1))
	     (cl-who:htm
	      (:li (:input :type "checkbox" :name safe)
		   (:label :for safe (cl-who:str name)))))
	    ((and (== b-type :points) (>= 5 points))
	     (cl-who:htm
	      (:li (loop for i from 0 repeat (+ points 1)
		      do (cl-who:htm (:input :type "radio" :name safe :value i)))
		   (:label :for safe (cl-who:str name)))))
	    ((== b-type :points)
	     (cl-who:htm
	      (:li (:input :type "text" :name safe :value "0")
		   (:label :for safe (cl-who:str name)))))
	    ((and (== b-type :range) (= min -1) (= max 1))
	     (cl-who:htm
	      (:li "TODO - up and downvote arrows")))
	    ((and (== b-type :range) (> 10 (- max min)))
	     (cl-who:htm
	      (:li (loop for i from min to max
		      do (cl-who:htm (:input :type "radio" :name safe :value i)))
		   (:label :for safe (cl-who:str name)))))
	    ((== b-type :range)
	     (cl-who:htm
	      (:li (:input :type "range" :name safe :min min :max max :value "0")
		   (:label :for safe (cl-who:str name)))))))))

(house:define-handler (election) ((election-id string))
  (assert (house:lookup :user house:session))
  (let* ((election (election-by-id (parse-integer election-id)))
	 (ballot-type (lookup election :ballot-type)))
    (page-template
      (:h3 (cl-who:str (lookup election :title)))
      (:form
       :action "/action/election/vote"
       (:ul :class "ballot"
	    (mapcar
	     (lambda (cand)
	       (candidate-input ballot-type cand))
	     (lookup election :candidates)))))))

(house:define-handler (election/manage) (election-id) :todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; main page
(house:define-handler (/) ()
  (page-template
    (if-let (user (house:lookup :user house:session))
      (cl-who:htm (:p "You are logged in as " (:b (cl-who:str (lookup user :name))))
		  (:ul
		   (:li (:a :href "/action/logout" "Logout"))
		   (:li (:a :href "/election/create" "Create Election"))))
      (cl-who:htm (:p "You are not logged in")
		  (:ul
		   (:li (:a :href "/auth/login" "Login"))
		   (:li (:a :href "/auth/register" "Register")))))))

;; (house:start 5555)
