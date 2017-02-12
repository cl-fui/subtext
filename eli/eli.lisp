(in-package :stext)
;;
;; eli is a keyprocessing entity
;;

(defclass eli ()
  ((state :accessor state)
   (keymap :accessor keymap :initarg :keymap :initform nil)
   (minibuf :accessor minibuf :initarg :minibuf :initform nil))
  
  )

(defmethod eli-reset ((eli eli))
  (with-slots (state keymap minibuf) eli
    (setf state keymap)
    (-reset minibuf)
    t))

(defun eli-inactive (eli)
  "Return t if eli is in the middle of a search"
  (with-slots (keymap state) eli
    (eq keymap state)))

(defmethod initialize-instance :after ((eli eli) &key)
  (with-slots (state keymap minibuf) eli
    (unless keymap (setf keymap (keymap-make)))
    (setf state keymap)
    ;; built-in bindings
    (eli-def eli (kbd "C-g") (lambda () (-reset eli)))
    ))

(defun process-key (eli key)
  "process a key with modifiers..."
  (with-slots (state keymap minibuf) eli
    (format t "eli:process-key ~A eli~A~&" (key-write key nil)
	    eli)
    (let ((found (key-lookup state key)))
      (print found)
      (typecase (cdr found)
	(function (eli-reset eli) ;reset search
		  (funcall (cdr found)))
	(cons (setf state found)
	      (key-write key minibuf)
	      (finish-output minibuf)
	      t)
	;;not found...
	(t
	 (if (eli-inactive eli)
	     nil
	     (eli-reset eli) ;cancel command and return t
	     ))))))

(defun eli-find (eli keyseq)
  "find a keyseq in this eli"
  (key-find (keymap eli) keyseq))

(defun eli-def (eli keyseq data)
  (key-def (keymap eli) keyseq data))

