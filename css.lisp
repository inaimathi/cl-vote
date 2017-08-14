(in-package #:cl-vote)

(define-handler (css/main.css :content-type "text/css") ()
  (css `((body :font-family sans-serif)

	 (.papers-panel :width 26% :margin-left 2% :float left)
	 (".papers-panel ul" :margin 0px :padding 0px))))
