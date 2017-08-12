;;;; package.lisp

(defpackage #:cl-vote
  (:use #:cl #:house #:cl-css #:cl-who #:parenscript)
  (:import-from #:anaphora :aif :it)
  (:shadow #:%))

(in-package :cl-vote)

(defparameter *dev-mode* nil)
(defparameter *http-port* nil)
(defparameter *thread* nil)
