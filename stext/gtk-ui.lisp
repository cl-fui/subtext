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
   
