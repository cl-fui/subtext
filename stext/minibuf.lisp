;;
;; A minibuf is a wtxt (text-view), which processes keys and
;; commands.  It has an rbuffer, and displays feedback and
;; specials instructions...

(in-package :stext )

(defclass minibuf (wtxt)
  ((keymap      :accessor keymap :initform (keymap-make))
   (keysearch   :accessor keysearch) ;state-machine pointer
   (frame       :accessor frame :initarg :frame)
   (lock        :accessor lock  :initform nil) ;lock input
   )
  
  (:metaclass gobject-class))

(defun make-minibuf (frame)
  (make-instance 'minibuf :buffer (make-instance 'rbuffer)
		 :frame frame :wrap-mode :none ))

(defmethod initialize-instance :after ((minibuf minibuf) &key)
  (setf (keysearch minibuf) (keymap minibuf)))

;;
;; Key processing
;;
;; If modifier, let gtk handle it to build a full key.
(defmethod -on-key-press ((view minibuf) event from)
  (let ((gtkkey (gdk-event-key-keyval event))
	(stream (gtk-text-view-buffer view))); need it for output
    (unless (modifier-p gtkkey); if modifier, let gtk handle it!
      (setf gtkkey (make-key gtkkey (gdk-event-key-state event)))
      (with-slots (keysearch keymap lock) view
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
	       lock))))))) ;if t, we are done.  nil means pass key on.








