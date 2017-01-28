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

(defun t1 ( &key (stdout *standard-output*))
  "a bare window containing a repl"  
  (within-main-loop
   ;; (setf *ui-thread* (bt:current-thread))
    (setf *standard-output* stdout) ;re-enable output
    (let ((window
	   (make-instance 'gtk-window
			  :type :toplevel
			  :title "t1"
			  :default-width 640
			  :default-height 480)) 
	  
	  (contents (make-instance 'gtk-box :orientation :vertical))
	  (scrolled (make-instance 'gtk-scrolled-window
				   :border-width 0
				   :hscrollbar-policy :automatic
				   :vscrollbar-policy :automatic))
	  (widj (make-wtxt (setf *pbuf* (make-instance 'swarepl;rbuffer
						       )))))	   
	  
      (g-signal-connect window "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  ;;			  (format t "~%DESTROY ~%")
			  (-on-destroy widj)
			  (leave-gtk-main)))
      (g-signal-connect window "key-press-event"
		      (lambda (window event)
			(-on-key-press widj event nil)))

      
      (gtk-container-add scrolled widj)
      (gtk-box-pack-start contents scrolled)
      (gtk-container-add window contents)
      (gtk-widget-show-all window))))
    ;; these are view-bound connects - do not put them in buffer!
    
    
    ;;(bind-keys *eli*)
    ;;(reset *eli* :full t)
   
(defun t2 ( &key (stdout *standard-output*))
  "window"
  (within-main-loop
   ;; (setf *ui-thread* (bt:current-thread))
    (setf *standard-output* stdout) ;re-enable output
    (let ((top
	   (make-instance 'gtk-window
			  :type :toplevel
			  :title "t1"
			  :default-width 640
			  :default-height 480)) 
	  (window (make-window
		   (make-wtxt
		    (setf *pbuf*
			  (make-instance 'swarepl))))))
	  
      (g-signal-connect top "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  ;;			  (format t "~%DESTROY ~%")
			  (-on-destroy window)
			  (leave-gtk-main)))
      (g-signal-connect top "key-press-event"
		      (lambda (top event)
			(-on-key-press window event nil)))

      
      (gtk-container-add top window)
      (gtk-widget-show-all top))))

(defun t3 ( &key (stdout *standard-output*))
  "final"
  (within-main-loop
   ;; (setf *ui-thread* (bt:current-thread))
    (setf *standard-output* stdout) ;re-enable output
    (let ((top (make-frame (make-window (make-wtxt (make-instance 'swarepl)))
			   :kill t))) 
      
      (gtk-widget-show-all top))))

;;for debugging ranges

(defgeneric present (it stream))
;------------------------------------------------
(defclass  ptest (range:range)
  ((toggle :accessor toggle :initarg :toggle :initform nil)
   (text1  :accessor text1  :initarg :text1  :initform nil)
   (num    :accessor num    :initarg :num    :initform nil)
   (text2  :accessor text2  :initarg :text2  :initform nil)))

(defparameter *tag* nil)
(defmethod  present ((p ptest) s)
  (with-slots (text1 text2 num toggle) p
    (unless toggle
	(progn
	  (with-tag s *tag*
	    (princ text1 s)
	    (with-tag s "output"
	      (princ num s))
	    (princ text2 s))))))


(defmethod  -on-button-press ((p ptest) iter event)
  (let ((buffer (gti-buffer iter)))
    (with-slots (toggle) p
      (setf toggle (not toggle))
      (mvb (start end) (range-iters buffer p)
	   (format t "~A ~A~&"start end)
					;(gtb-apply-tag buffer "input" start end)
	   (gti-backward-char end)
	   (%gtb-delete buffer start end)
	   (present p buffer)
	   )
      )))
(defparameter *top* nil)
(defun t0 ( &key (stdout *standard-output*))
  "final"
  (within-main-loop
   ;; (setf *ui-thread* (bt:current-thread))
    (setf *standard-output* stdout) ;re-enable output
    (let ((buffer (make-instance 'rbuffer)))
      (setf *pbuf* buffer)
      (let ((top (make-frame (make-window (setf *top* (make-wtxt buffer)))
			     :kill t))
	    r) 
	(with-range buffer (range:make)
	  (format buffer "hello~&"))
	(setf *tag* (gttt-lookup (gtb-tag-table buffer) "prompt" ))
	(time
	 (loop for i from 1 to 10000 do
	      (with-range buffer (make-instance 'ptest :text1 "hello" :num i :text2 "world")
		(present it buffer)
		(terpri buffer))
	      ))
	
	(finish-output buffer)
	(gtk-widget-show-all top)))))
;;WATCH OUT!
(defparameter *q* nil)

(defmethod -on-button-press ((buffer rbuffer) iter event)
  (stream-to-iter buffer iter)
  (let ((range  (range:at (root buffer) (gti-get-offset iter))))
    (-on-button-press range iter event))
  (print "1"))
(defmethod -on-2button-press ((buffer rbuffer) iter event)
  (print "2"))
(defmethod -on-3button-press ((buffer rbuffer) iter event)
  (print "3"))
