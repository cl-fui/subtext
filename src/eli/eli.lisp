(in-package :subtext)
;;
;; eli is a state machine for processing keys.  Every character that comes in
;; may push a new state (which is a pointer into the keymap)
;;

(defclass eli ()
  ((state  :accessor state
	   :documentation "first= binding during search, rest are previous bindings")
   (keymap :accessor keymap :initarg :keymap :initform (list nil))))

(defmethod initialize-instance :after ((eli eli) &key)
  (eli-init-keynames);; global - keynames
  (with-slots (state keymap) eli
    (setf state (cons keymap nil)))
  )
(defun eli-reset (eli)
  (with-slots (state keymap) eli
    (setf state (cons keymap nil))
    ;(terpri *echo*)
    t))

(defun eli-state-print(eli stream)
  (labels ((printer (state stream)
	     (and (cdr state)
		(printer (cdr state) stream))
	   (and (car (car state))
		(progn (key-write (car (car state)) stream)
		       (write-char #\space stream)))))
    (terpri stream)
    (printer (state eli) stream)))

(defun eli-active (eli)
  "Return t if eli is in the middle of a search"
  (with-slots (keymap state) eli
    (not (eq keymap (car state)))))


;;
;; active  found
;;   0       0      pass on
;;   0       1      eli
;;   1       0      cancel
;;   1       1      eli

;;todo: immediate keys

(defun process-key (eli key event)
  "process a key with modifiers..."
  (format t "ELI:PROCESSKEY  ~A~&" key )
  (with-slots (state keymap) eli
    (if (= key #x1000067); first, proces C-g
	(progn (eli-reset eli) (format *echo* "~%Quit" ) t)
	(let ((found (key-lookup (car state) key)))
	  (if found
	      (typecase (cdr found)
		(function (eli-reset eli) ;reset search#
			  (funcall (cdr found) ))
		(cons (push found state)
		      (eli-state-print eli *echo*)
		      t))
	      ;;not found...
	      (if (eli-active eli)
		  (progn
		    (eli-state-print eli *echo*)
		    (key-write key *echo*)
		    (with-tag ("error" *echo*)
		      (format *echo* " NOT BOUND"))
		    (eli-reset eli))
		  (progn
		    nil)))))))

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
  (key-def (keymap eli) keyseq data))



