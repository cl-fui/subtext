(in-package :eli)
;;
;; eli is a keyprocessing entity
;;

(defclass eli ()
  ((state :accessor state)
   (keymap :accessor keymap :initarg :keymap :initform nil) ) )

(defmethod -reset ((eli eli))
  (with-slots (state keymap) eli
    (setf state keymap)))

(defmethod initialize-instance :after ((eli eli) &key)
  (with-slots (state keymap) eli
    (unless keymap (setf keymap (keymap-make)))
    (setf state keymap))
  )

(defun process-key (eli key)
  "process a key with modifiers..."
  (with-slots (state keymap) eli
    (format t "eli:process-key ~A eli~A~&" (key-write key nil)
	    eli)
    (let ((found (key-lookup state key)))
      (print found)
      (typecase (cdr found)
	(function (-reset eli) ;reset search
		  (funcall (cdr found)))
	(cons (setf state found)
	      t)
	(t (-reset eli) 
	   nil)))))

(defun find (eli keyseq)
  "find a keyseq in this eli"
  (key-find (keymap eli) keyseq))

(defun def (eli keyseq data)
  (key-def (keymap eli) keyseq data))
