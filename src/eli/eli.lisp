(in-package :subtext)
;;




(defclass eli ()
  ((keymap :accessor keymap :initform nil)
   (state  :accessor state :initform (make-array 20 :adjustable t :fill-pointer 0)
	   :documentation "first= binding during search, rest are previous bindings")
  ))

(defmethod initialize-instance :after ((eli eli) &key)
  (eli-init-keynames))

(defun eli-reset (eli ret)
  (with-slots (state) eli
    (setf (fill-pointer state) 0)
    ret))

(defun eli-error (eli &key (msg nil) (newline nil))
  (eli-state-print eli *echo*)
  (and newline (terpri *echo*))
  (and msg (with-tag ("error" *echo*)  (princ msg *echo*)))
  (eli-reset eli t))


(defun eli-state-print(eli stream)
  "print the state of our keyseq and return t"
  (terpri stream)
  (keyseq-write (state eli) stream)
  t)

(defun eli-active (eli)
  "Return t if eli is in the middle of a search"
 ; (format t "ELI-ACTIVE: ~A~&" (fill-pointer (state eli)))
  (not (= 1 (fill-pointer (state eli)))))

(defun eli-key-initial (eli gtkkey modifiers)
  "Initial processing of a key.  Return nil if done, or keyseq to continue"
  (if (key-is-modifier gtkkey)
      nil
      (let ((key (key-make gtkkey modifiers)))
	(if (= key #x1000067); first, proces C-g
	    (eli-error eli :msg "Quit" :newline t)
	    (with-slots (state) eli
	      (vector-push key state)
	      state)))))

;; keys here have to execute in a nil context...
(defun eli-key-final (eli subtext result)
  "process result of keyseq work.  Result is one of:
nil     = done, no further processing required;
integer = partials found"
  (and result (setf result
		    (keymap-process (keymap eli)
				    (state eli)
				    subtext
				    nil
				    result))) ; last-ditch
  (if result
      ;; result is the count of partial matches
      (if (zerop result)
	  ;;no match found! single key or a keyseq?
	  (if (eli-active eli); for an active keyseq, error
	      (eli-error eli :msg " NOT BOUND");T
	      (eli-reset eli nil));NIL - single key, let GTK process.
	  ;; partial match in any event means T and continue accumulating.
	  (eli-state-print eli *echo*))
      ;;nil result means we are done.
      (eli-reset eli t)))

(defun eli-process (eli found partials)
  "process a found binding"
  ;; make a last, desperate attempt to find a binding here in eli's
  ;; last resort keymap...
  (unless found
    (multiple-value-setq (found partials)
      (keymap-find (keymap eli) (state eli) partials)))

  (format t "ELI-PROCESS: found ~A ~A~&" found partials)
  (if found
      (eli-reset eli (funcall (cdr found)))
      (if (zerop partials)
	  (if (eli-active eli)
	      (eli-error eli :msg " NOT BOUND")
	      (eli-reset eli nil))
	  (progn
	    (eli-state-print eli *echo*) t); some partials.  Eat key and continue
	  )))



(defun eli-def (eli keyseq data)
  "bind a keyseq in eli"
  (keymap-def (keymap eli) keyseq data))


