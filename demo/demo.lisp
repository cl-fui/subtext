(in-package :subtext)
;;TODO: this is kind of generic...
(defpres button (pres) (code))
(defmethod -pres-on-button ((p button) button)
  (format t "Package ~A~&" *package*)
  (funcall (code p)))



(defun demo ()
  (let ((so *standard-output*)
	(pkg *package*))
    (within-main-loop
      (setf *standard-output* so); Why can't I just bind it?
      (format t "~A ~A~&" *standard-output* so)
      (let* ((*standard-output* so)
	     (*package* pkg)			;re-enable output
	     stream eli
	     ;;	   (ass  (format t "STANDARD OUTPUT?~A ~A ~&"*standard-output* *package*))
	     (top (make-frame (make-window
			       (setf eli (make-rview
					  (setf stream (make-instance 'termstream))
					  :widget-defaults nil
					  :wrap-mode :word
					  :left-margin 30 :right-margin 20 
					  ))
			      ;; :ml ""
			       )
			      :width 640 :height 480
			      :title "Welcome to SubText."
			      :kill t)))
	(pbuf-new-tag stream :name "small" :font "DejaVu Sans 10")
	
	(gtk-widget-modify-font eli  (pango-font-description-from-string "DejaVu Serif 12"))
	(gtk-widget-show-all top)
	(-on-initial-display top)

	(eli-def eli (kbd "Mouse-1")
	     (lambda ();; (format t "~A ~A~&" (x eli) (y eli))
	       (with-slots (x y) eli
		 (let* ((iter (rview-iter-from-xy eli x y))
			(presentations (presentations-at stream iter)))
		   (loop for pres in presentations
		      until (-pres-on-button pres 1))))))
	(eli-def eli (kbd "C-x C-c")
		 (lambda ()
		 	 (format t "HERE")
			 (gtk-widget-destroy top)
			 ))
	
	(pres-tag stream button (:foreground "DarkGoldenrod" :background "aquamarine" :editable nil)  )

	(terpri stream)
	(prin stream "Welcome to " (tg "bg-greenish" "SubText™") #\. #\newline #\newline
	      "SubText is an initiative to create a Lispy, mostly-text-based user interface.

'Entangling' runs of text with CL code makes text interactive.  Combined with an Emacs-like command processing system¹, SubText enables the creation of simple, flexible and familiar ad-hoc user interfaces, with decent antialiased fonts.

SubText is in its infancy, and at this stage is changing rapidly.  The demos below are a work in progress and are not fully functional... Tested with SBCL.

Click on "
	      (pr 'button (:code (lambda () (news))) "NEWS  DEMO")
	      " to display a view of a newsgroup with more than half a million posts; scroll around to see what's there.

Start a SWANK server on port 5000 and try our "
	      (pr 'button (:code (lambda () (repl))) "REPL")
	      " without any Emacs!"
	      " Evaluate some Lisp code or some nonsense to invoke the SubText debugger...


"
	      (tg "small" "¹ Feel free to use C-x C-c to quit..." ))
	(finish-output stream))
      )))
