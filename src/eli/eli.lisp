(in-package :subtext)
;;
;; eli is a state machine for processing keys.  Every character that comes in
;; may push a new state (which is a pointer into the keymap)
;;

(defclass eli ()
  ((state  :accessor state :initform (make-array 20 :adjustable t :fill-pointer 0)
	   :documentation "first= binding during search, rest are previous bindings")
   (keymap :accessor keymap :initarg :keymap :initform (make-instance 'keymap))))

(defmethod initialize-instance :after ((eli eli) &key)
  (eli-init-keynames))

(defun eli-reset (eli)
  (with-slots (state) eli
    (setf (fill-pointer state) 0)
    t))

(defun eli-error (eli &key (msg nil) (newline nil))
  (eli-state-print eli *echo*)
  (and newline (terpri *echo*))
  (and msg (with-tag ("error" *echo*)  (princ msg *echo*)))
  (eli-reset eli))


(defun eli-state-print(eli stream)
  (terpri stream)
  (keyseq-write (state eli) stream))

(defun eli-active (eli)
  "Return t if eli is in the middle of a search"
 ; (format t "ELI-ACTIVE: ~A~&" (fill-pointer (state eli)))
  (not (= 1 (fill-pointer (state eli)))))


;;
;; active  found
;;   0       0      pass on
;;   0       1      eli
;;   1       0      cancel
;;   1       1      eli

;;todo: immediate keys

(defun process-key (eli key event)
  "process a key with modifiers..."
  (with-slots (state keymap) eli
    (if (= key #x1000067); first, proces C-g
	(eli-error eli :msg "Quit" :newline t)
	;; First, append key and try to find the keyseq.
	(progn
	  (vector-push key state)
	  (mvb (found partials) (keymap-find keymap state)
	      ;; (keymap-dump keymap)	  
	       (if found
		   (prog1 (funcall (cdr found))
		     (eli-reset eli))
		   ;;not found...
		   (if (zerop partials); no chance of finding?
		       (if (eli-active eli) ;if active, cancel
			   (eli-error eli :msg " NOT BOUND")	;t
			   (progn (eli-reset eli) nil));if inactive, pass it on
		       ;; partials...
		       (progn
			 (eli-state-print eli *echo*)
			 t))))))))

(defun eli-gtk-key-press-event (eli event)
  "Process a gtk event; passed to GTK as a callback"
  (let ((gtkkey (gdk-event-key-keyval event)))
    (unless (key-is-modifier gtkkey)	; if modifier, let gtk handle it!
      (let ((key (key-make gtkkey (gdk-event-key-state event))))
	(process-key eli key event) ))) )

(defun eli-find (eli keyseq)
  "find a keyseq in this eli"
  (key-find (keymap eli) keyseq))

(defun eli-def (eli keyseq data)
  "bind a keyseq in eli"
  (keymap-def (keymap eli) keyseq data))



