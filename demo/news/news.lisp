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
(defmethod -con-mouse-enter (subtext (context pedition) priority)
  (with-slots (out edition) context
    (with-slots (iter iter1) out
      (context-bounds out context)
      (progn
	(gtb-apply-tag  out "grhigh" iter iter1)
	(-reset *echo*)
	(if (edition-full? edition)
	    (format *echo* "All ~A parts" (edition-count-parts edition))
	    (format *echo* "~A of ~A parts"
		    (edition-count-parts edition)
		    (length (edition-parts edition))))
	(format *echo* " posted by ")
	(with-tag ("prompt" *echo*) (princ (edition-poster edition) *echo*))
	(promises-fulfill *echo*);todo: clean up
	  ))))

(defmethod -con-mouse-exit (subtext (context pedition) priority)
  (with-slots (out edition) context
    (with-slots (iter iter1) out
      (context-bounds out context)
      (gtb-remove-tag out "grhigh" iter iter1))))



(defun news ()
  (within-main-loop
    (let* (stream
	   ;;	   (ass  (format t "STANDARD OUTPUT?~A ~A ~&"*standard-output* *package*))
	   (top (make-frame (make-window (make-rview
					  (setf stream (make-instance 'conbuf))))
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
      (idly
       (group-load *g*)
       (format *echo* "Done")
       (loop while (gtk-events-pending)
	  do(gtk-main-iteration))
       (loop for edition in (group-data *g*) 
	  for i from 1 do
	    (when (= 50 (mod i 1000))
	      (finish-output stream)
	      (loop while (gtk-events-pending)
		 do(gtk-main-iteration)))
	    (format stream "~3d " i)
	    (present (make-instance 'pedition :edition edition))
	    (terpri stream)
	    )
       (finish-output stream)))))
