(in-package #:cl-vote)

(defun universal->string (universal-time)
  (when universal-time
    (local-time:format-timestring
     nil (local-time:universal-to-timestamp universal-time)
     :format local-time:+RFC3339-FORMAT/DATE-ONLY+)))
