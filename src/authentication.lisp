(in-package :cl-vote)
(named-readtables:in-readtable clj:syntax)

(setf *random-state* (make-random-state t)
      crypto:*prng* (crypto:make-prng :fortuna)
      totp:*time-step-in-seconds* 30
      hotp:*digits* 6)

(defparameter *generator*
  (session-token:make-generator
   :alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
   :token-length 6))

(defun mk-recovery-token ()
  (format
   nil "~a-~a-~a"
   (funcall *generator*)
   (funcall *generator*)
   (funcall *generator*)))

(defun mk-otp-secret ()
  (ironclad:byte-array-to-hex-string (crypto:random-data 256)))

(defun mk-user (name &key secret)
  {:name name :secret (or secret (mk-otp-secret))})

(defun user-name (user) (lookup user :name))

(defun user-secret (user) (lookup user :secret))

(defun user-otp-uri (user)
  (format nil "otpauth://totp/cl-vote:~A?secret=~A&issuer=2FATest&digits=~a"
	  (user-name user)
	  (cl-base32:bytes-to-base32
           (ironclad:hex-string-to-byte-array
            (user-secret user)))
	  hotp:*digits*))

(defun user-qrcode (user)
  (flex:with-output-to-sequence (stream)
    (cl-qrencode:encode-png-stream
     (user-otp-uri user) stream
     :level :level-m
     :mode :byte)))

(defun user-authorized? (user token)
  (ignore-errors
    (= (totp:totp (user-secret user))
       (parse-integer token))))
