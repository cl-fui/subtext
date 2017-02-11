;;
;; A minibuf is an rview (text-view), which processes keys and
;; commands on behalf of another entity, the main window.

(in-package :stext )
;;==============================================================================
(defclass minibuf (rview)
  ((keymap  :accessor keymap :initform (eli:keymap-make))
   (state   :accessor state) ;state-machine binding pointer
   (frame   :accessor frame :initarg :frame))  
  (:metaclass gobject-class))

(defun make-minibuf (frame)
  (make-instance 'minibuf :buffer (make-instance 'termstream)
		 :frame frame :wrap-mode :none ))

(defmethod initialize-instance :after ((minibuf minibuf) &key)
  (setf (state minibuf) (keymap minibuf)))

;;
;; Key processing
;;
;; Key is an eli-processed gtkkey with modifiers.
;; Generally, look up a key and call its (lambda keycode stream)
;; 
;; 
(defmethod -on-eli-key ((view minibuf) key event)
  "process a key with modifiers..."
  (declare (ignore event))
      (format t "minibuf:-on-eli-key ~A~&" (eli:key-write key nil))
  (with-slots (state keymap lock) view
    (let ((found (eli:key-lookup state key))
	  (stream (gtv-buffer view)));local eli buffer
      (typecase (cdr found)
	(function (setf state keymap) ;reset search
		  (stream-wipe stream);local
		  (funcall (cdr found) (car found)))
	(cons (setf state found)
	      (eli:key-write (car found) stream)
	      (finish-output stream)
	      t)
	(t (setf state keymap)
	   (stream-wipe stream)
	   nil)))))









