;;; ----------------------------------------------------------------------------
;; (ql:quickload :present)(in-package :present)(test)
;; (progn (push  (authors-view *g*) *view*) nil)
;; (progn (push  (groups-view *g*) *view*) nil)
;;; ----------------------------------------------------------------------------
(in-package #:stext)

(defgeneric -on-destroy (obj))
(defmethod  -on-destroy ((obj t) ))
(defgeneric -on-key-press (obj event from))
(defmethod  -on-key-press ((obj t) event from) nil) ;default: pass key on

(defparameter *pbuf* nil)

;;for debugging ranges

(defgeneric present (obj stream extra))
;------------------------------------------------
#||

(defmethod  present ((p ptest) stream other)
  (with-slots (text1 text2 num toggle) p
    (unless toggle
      (progn
	(with-tag *tag*
	  (princ text1 stream)
	  (with-tag "output"
	    (with-range stream (make-instance 'pfake)
	      (format stream "~A"  num)))
	  (princ text2 stream))))))
||#
(defparameter *r* nil)
(defun t4 ( &key (stdout *standard-output*))
  "final"
  (within-main-loop
   ;; (setf *ui-thread* (bt:current-thread))
    (setf *standard-output* stdout) ;re-enable output
    (let ((top (make-frame (make-window (make-rview
					 (setf *pbuf*
					       (make-instance 'termstream)))) 
			   :kill t))) 
      (gtk-widget-show-all top)
      (g-idle-add
       (lambda ()
	 (let* ((stream *pbuf*)
	       (tag-error (pbuf-find-tag stream "error" )))
	   (format stream "------")
	   (with-tag (make-instance 'pres :tag "error")
	     (setf *r* (with-tag  (make-instance 'pres :tag "prompt")
			 (format stream "HELL ")
			 
			 (with-tag (make-instance 'pres :tag "pres")
			   (format stream "fuck "))

			 (format stream "_AND_")
			 (with-tag (make-instance 'pres :tag "input")
			   (format stream "shit ")))))

	   (format stream "WHA? ")
	   (format stream "DONE.")
	   (finish-output stream)
	   

	   ;;(file-position stream (range:bounds *r* ))

	   ;;(with-range stream (make-instance 'pres3)   (format stream "|OK, MAN|~&"))  (finish-output stream)
	   
	   (-on-initial-display top))
	 nil)))))
 

;; text new pmark system...

(defparameter *top* nil)
(defparameter *tag* nil)
(defclass p1 (pres) ())
(defclass p2 (pres) ())

(defun t10 ( &key (stdout *standard-output*))
  "final"
  (within-main-loop
   ;; (setf *ui-thread* (bt:current-thread))
    (setf *standard-output* stdout) ;re-enable output
    (let ((buffer (make-instance 'rbuffer)))
      (setf *pbuf* buffer)
      (let ((top (make-frame (make-window (setf *top* (make-rview buffer)))
			     :kill t))
	    r)
	(gtk-widget-show-all top)
	
	(format t "SHOWING~&")
	;;	(with-range buffer (range:make)	  (format buffer "hello~&"))


	(let ((tag-prompt (pbuf-find-tag buffer "prompt"))
	      (tag-pres   (pbuf-find-tag buffer "pres")))
	 	  


	  (let ((stream buffer))
	    (time
	     (loop for i from 1 to 10 do
		;;(with-range buffer)
		;;(stream  (make-instance 'ptest :text1 "hello" :num i :text2 "world" )(present it buffer nil))
		  (with-tag (make-instance 'p1 :tag tag-prompt)
		    (format buffer "Hello "))
		  
		  (with-tag (make-instance 'p2 :tag tag-pres)
		    (format buffer "world"))
		  
		  (format buffer "...~&")
		;;	(terpri buffer)
		  ))))	
	(finish-output buffer))
      
      
      )))


(defpres p3 (pres) :tag (:foreground "blue"))
(defpres p4 (pres) :tag (:foreground "green"))


(defun t11 ( &key (stdout *standard-output*) (package *package*))
  "final"
  (within-main-loop
   ;; (setf *ui-thread* (bt:current-thread))
    (setf *standard-output* stdout) ;re-enable output
    (setf *package* package)
    (let ((*standard-output* stdout)
	  (*package* package)
	  (buffer (make-instance 'rbuffer)))
      (setf *pbuf* buffer)
      (let ((top (make-frame (make-window (setf *top* (make-rview buffer)))
			     :kill t))
	    r)
	(gtk-widget-show-all top)
	(pres-in-buffer buffer 'p3)
	(pres-in-buffer buffer 'p4)
	(format t "SHOWING~&")
	;;	(with-range buffer (range:make)	  (format buffer "hello~&"))

	(let ((stream buffer))
	  (time
	   (loop for i from 1 to 10 do
	      ;;(with-range buffer)
	      ;;(stream  (make-instance 'ptest :text1 "hello" :num i :text2 "world" )(present it buffer nil))
		(with-pres p4
		  (format buffer "Hello "))
		
		(with-pres p3
		  (format buffer "world"))
		
		(format buffer "...~&")
	      ;;	(terpri buffer)
		)))	
	(finish-output buffer))
      
      
	)))

;;;-----------------------------------------------------------------------------------
;;;
;;; SWANK REPL

(defun t3 ( &key (stdout *standard-output*) (package *package*))
  "final"
  (within-main-loop
    ;; (setf *ui-thread* (bt:current-thread))
    (let* ((*standard-output* stdout)
	  (*package* package)			;re-enable output
	  (top (make-frame (make-window (make-rview (make-instance 'swarepl))) 
			   :kill t))) 
	
      (gtk-widget-show-all top)
      (g-idle-add
       (lambda ()
	 (-on-initial-display top)
	 nil)))))



(defun t99 (&key (stdout *standard-output*) (package *package*))
  (within-main-loop
   ;; (setf *standard-output* stdout)
    (let* ((*standard-output* stdout);; (*package* package)
	   (win (make-instance 'gtk-window :type :toplevel :default-width 640 :default-height 480)))
      (g-signal-connect win "destroy" (lambda (widget) (print widget) (leave-gtk-main)))
      (g-signal-connect win "key-press-event" (lambda (widget event) (print event) nil))
      (gtk-widget-show-all win)
      (print *package*))
    ))

(defun t100 ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
			  
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-widget-show-all window))))
