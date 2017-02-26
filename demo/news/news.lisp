(in-package :subtext)

(defcontext pedition (ctx) 
  ((edition :accessor edition :initarg :edition)) )


(defpresenter ((p pedition))
  (with-slots (edition) p
    (format out "~4d " (edition-count-parts edition))
    (if (edition-full? edition)
	(format out " ~A" (edition-name edition)) 
	(with-tag ("incomplete" out)
	  (format out " ~A" (edition-name edition)) ))))

;;highlight pedition
(defmethod  -pres-on-mouse ((pres pedition) flag)
  (with-slots (out) pres
    (with-slots (iter iter1) out
      (context-bounds out pres)
      (if flag
	  (progn
	    (gtb-apply-tag  out "grhigh" iter iter1)
	    (with-slots (edition) pres
	      (-reset *echo*)
	      (if (edition-full? edition)
		  (format *echo* "All ~A parts" (edition-count-parts edition))
		  (format *echo* "~A of ~A parts"
			  (edition-count-parts edition)
			  (length (edition-parts edition))))
	      (format *echo* " posted by ")
	      (with-tag ("prompt" *echo*) (princ (edition-poster edition) *echo*))
	      (promises-fulfill *echo*);todo: clean up
	      ))
	  (gtb-remove-tag out "grhigh" iter iter1))))
  t)

(defun news ()
  (let ((so *standard-output*)
	(pkg *package*))
    (within-main-loop
      (setf *standard-output* so); Why can't I just bind it?
      (format t "AHA: OUTPUT ~A ~A~&" *standard-output* so)
      (format t "~A ~A~&" *package* pkg)
      (let* ((*standard-output* so)
	     (*package* pkg)			;re-enable output
	     (stream)
	     ;;	   (ass  (format t "STANDARD OUTPUT?~A ~A ~&"*standard-output* *package*))
	     (top (make-frame (make-window (make-rview
					    (setf stream (make-instance 'mark-out-stream))))
			      :title "NNTP News Demo"
			      :kill t)))
	(context-tag stream pedition (:foreground "LightBlue" :editable nil)  )
	(pbuf-new-tag stream :name "incomplete"
		      :foreground-rgba (gdk-rgba-parse "Black")
		      :background-rgba (gdk-rgba-parse "Red"))
	(pbuf-new-tag stream :name "grhigh" :foreground "Blue" :background "yellow" )
	

	(gtk-widget-show-all top)
	(-on-initial-display top)


	(-wipe *echo*)
	(format *echo* "Loading 10,753 records representing 579,328 NNTP posts...")
	(bt:make-thread
	 (lambda ()
	   (let ((*package* pkg))
	     (group-load *g*)
	     (gsafe (format *echo* "Done"))
	     (gsafe (loop while (gtk-events-pending) do(gtk-main-iteration)))
	     (loop for edition in (group-data *g*) 
		for i from 1 do
		  (when (= 50 (mod i 1000))
		    (gsafe (finish-output stream)
			   (loop while (gtk-events-pending) do(gtk-main-iteration))))
		  (gsafe
		   (format stream "~3d " i)
		   (present (make-instance 'pedition :edition edition))
		   (terpri stream))
		  )
	     (gsafe (finish-output stream)))))
   
     (finish-output stream)))))
