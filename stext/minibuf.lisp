(in-package :stext )

(defclass minibuf (wtxt) () (:metaclass gobject-class))

(defun make-minibuf ()
  (make-instance 'minibuf :buffer (make-instance 'rbuffer) :wrap-mode :none ))

(defmethod initialize-instance :after ((minibuf minibuf) &key)
 
  )

(defmethod -on-key-press ((view minibuf) event from) 
  (let ((stream (gtk-text-view-buffer view)))
    (format stream "~C" (code-char (gdk-event-key-keyval event)))
    (finish-output stream)
    (-on-key-press from event nil)))




