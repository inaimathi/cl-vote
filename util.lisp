(in-package #:cl-vote)

(defun universal->string (universal-time)
  (when universal-time
    (local-time:format-timestring
     nil (local-time:universal-to-timestamp universal-time)
     :format local-time:+RFC3339-FORMAT/DATE-ONLY+)))

(defun hash (&rest k/v-pairs)
  (let ((h (make-hash-table)))
    (loop for (k v) on k/v-pairs by #'cddr
       do (setf (gethash k h) v))
    h))
