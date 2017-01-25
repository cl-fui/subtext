(in-package :stext )

(defclass minibuf (wtxt)
  ((keymap :accessor keymap :initform (keymap-make))
   (keysearch :accessor keysearch )
   (frame :accessor frame :initarg :frame))
  (:metaclass gobject-class))

(defun make-minibuf (frame)
  (make-instance 'minibuf :buffer (make-instance 'rbuffer)
		 :frame frame :wrap-mode :none ))

(defmethod initialize-instance :after ((minibuf minibuf) &key)
  (setf (keysearch minibuf) (keymap minibuf)))

(defmethod -on-key-press ((view minibuf) event from)
  (let ((gtkkey (gdk-event-key-keyval event))
	(stream (gtk-text-view-buffer view)))
    (unless (modifier-p gtkkey); if modifier, let gtk handle it!
      (setf gtkkey (make-key gtkkey (gdk-event-key-state event)))
      (with-slots (keysearch keymap) view
	(let ((found (keymap-lookup keysearch gtkkey)))
	  (typecase (cdr found)
	    (function (setf keysearch keymap)
		      (stream-wipe stream)
		      (funcall (cdr found)))
	    (cons (setf keysearch found)
		  (princ (key->string (car found)) stream)
		  (finish-output stream)
		   t)
	    (t (setf keysearch keymap)
	       (stream-wipe stream)
	       nil))))))  
)








