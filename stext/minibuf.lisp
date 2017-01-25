(in-package :stext )

(defclass minibuf (wtxt) () (:metaclass gobject-class))

(defun make-minibuf ()
  (make-instance 'minibuf :buffer (make-instance 'rbuffer) :wrap-mode :none ))

(defmethod initialize-instance :after ((minibuf minibuf) &key)
 
  )

(defmethod -on-key-press ((view minibuf) event from)
  (let ((code (gdk-event-key-keyval event) ))
    (case code
      (#xFF0D 
       (let ((iter (gtb-get-iter-at-mark pbuf (gtb-get-insert pbuf))))
	 ;; only allow input at the end!
	 (if (gti-is-end iter)
	     (gdk-threads-add-idle #'pbuf-idle-entry))))
      (t (write-char (code-char code) pbuf)
	 (finish-output pbuf))))


  
  
  (let ((stream (gtk-text-view-buffer view)))
    (format stream "~C" (code-char (gdk-event-key-keyval event)))
    (finish-output stream)
    (-on-key-press from event nil)))








