(in-package :subtext)
;;TODO: this is kind of generic...
(defcontext button (ctx) (code))
(defkeymap button)


(defmethod -pres-on-button ((p button) button)
  (format t "Package ~A~&" *package*)
  (funcall (code p)))


(defun demo ()
  (clrf bt:*default-special-bindings*)
  (push (cons '*standard-output* *standard-output*) bt:*default-special-bindings*)
  (push '(*package* find-package :subtext) bt:*default-special-bindings*)
  (print bt:*default-special-bindings*)
  (gtk::within-main-loop
    (let* (stream
	   eli
	   (top (make-frame (make-window
			     (setf eli (make-rview
					(setf stream (make-instance 'conbuf))
					:widget-defaults nil
					:wrap-mode :word
					:left-margin 30 :right-margin 20
					:pixels-above-lines 24
					)))
			    :width 640 :height 480
			    :title "Welcome to SubText."
			    :kill t)))
      (pbuf-new-tag stream :name "small" :font "DejaVu Sans 10")
      (pbuf-new-tag stream :name "normal" :pixels-inside-wrap 8)
      (gtk-widget-modify-font eli  (pango-font-description-from-string "DejaVu Serif 12"))
      (gtk-widget-show-all top)
      (-on-initial-display top)

      (eli-def eli (kbd "C-x C-c")
	       (lambda (subtext context) (gtk-widget-destroy top)))
      (clrf keymap-button)
      (keymap-def keymap-button (kbd "Mouse-1") (lambda (subtext button)  (funcall (code button)) nil))
      (format t "FFF: OUTPUT ~A ~A~&" *standard-output* *package*)
      
      (context-tag stream button (:foreground "DarkGoldenrod" :background "aquamarine" :editable nil)  )

      (prin stream "Welcome to " (tg "bg-greenish" "SubText™") #\. #\newline
	    "SubText is a Lispy, mostly-text-based user interface.
SubText 'entangles' CL code with runs of text, enabling simple, flexible and familiar¹ ad-hoc user interfaces.  With decent antialiased fonts.
SubText is in its infancy; the demos below are a work in progress.
Click on "
	    (pr 'button (:code (lambda () (news))) "NEWS  DEMO")
	    " to display a view of a newsgroup with more than half a million posts; scroll around to see what's there.
Start a SWANK server on port 5000 and try our "
	    (pr 'button (:code (lambda () (repl))) "REPL") " "
	    (tg (pbuf-new-tag stream :underline :single) "without any Emacs") 
	    "! Evaluate some Lisp code (or some nonsense to invoke the SubText debugger)..." #\newline 
	    (tg "small" "¹ Feel free to use C-x C-c to quit..." ))
      (finish-output stream))
    ))
