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
  (terpri stream)
  (keyseq-write (state eli) stream))

(defun eli-active (eli)
  "Return t if eli is in the middle of a search"
 ; (format t "ELI-ACTIVE: ~A~&" (fill-pointer (state eli)))
  (not (= 1 (fill-pointer (state eli)))))

(defun eli-key-initial (eli key)
  "Initial processing of a key.  Return T if processed, or nil to continue"
  (with-slots (state) eli
    (if (= key #x1000067); first, proces C-g
	(eli-error eli :msg "Quit" :newline t)
	(prog0 (vector-push key state)))))

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



