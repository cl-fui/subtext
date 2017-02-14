;;
;; A minibuf is an rview (text-view), which processes keys and
;; commands on behalf of another entity, the main window.

(in-package :subtext )
;;==============================================================================
(defclass minibuf (rview)
  (   (frame   :accessor frame :initarg :frame))  
  (:metaclass gobject-class))

(defun make-minibuf (frame)
  (make-instance 'minibuf :buffer (make-instance 'echostream)
		 :frame frame :wrap-mode :none ))

(defmethod initialize-instance :after ((minibuf minibuf) &key)
  (setf *echo* (gtv-buffer minibuf))
  )

;;
;; Key processing
;;
;; Key is an eli-processed gtkkey with modifiers.
;; Generally, look up a key and call its (lambda keycode stream)
;; 

;; 










