;; 
;; ConBuf
;;
;; Situated above the stream and the buffer, this class coordinates context
;; actions.
;;
;;
(in-package :subtext)

;;;=============================================================================
;;; tb - An augmented gtk text buffer with a position-tracking system.
;;;
(defclass conbuf (mark-out-stream)
  ((cursorcons  :accessor cursorcons :initform nil
	        :documentation "contexts under cursor")
   (pointercons :accessor pointercons :initform nil
		:documentation "contexts under pointer"))
  (:metaclass gobject-class))

;;------------------------------------------------------------------------------
(defmethod initialize-instance :after ((buffer conbuf) &key )
  (g-signal-connect
     buffer "notify::cursor-position"
     (lambda (gobject gparamspec)
       (declare (ignore gobject gparamspec))
       (with-slots (iter) buffer
	 (%gtb-get-iter-at-offset buffer iter (gtb-cursor-position buffer))
	 (on-cursor-position buffer iter)))))


;; A useful primitive.  As cursor and mouse pointer move around, we enter and
;; exit contexts.  contexts-update fetches the contexts we are in and compares
;; them against the old context list.  Any newly-intered contexts and newly-
;; exited contexts are then updated with appropriate functions.
(defun contexts-update (buffer iter old fexit fenter)
  "get contexts we are in and using old list as reference, notify freshly 
entered or exited contexts.  Return new context list"
  (let* ((new (contexts-at buffer iter)) 
	 (same (intersection old new)) ; these have not changed...
	 (out (set-difference old same)); these are phased out.
	 (in  (set-difference new same))) ; and these are newly introduced.
	 (format t "NEW: ~A~&OLD:~A~&" new old)
    (loop for context in out
       for i from 0 do (funcall fexit context i))
    (loop for context in in
       for i from 0 do (funcall fenter context i))
    new))

;;------------------------------------------------------------------------------
;; Cursor Position...
;;
(defun on-cursor-position (buffer iter)
  (with-slots ((old cursorcons)) buffer
    (setf old
	  (contexts-update buffer iter old
			   #'-con-exit
			   #'-con-enter))))
;;------------------------------------------------------------------------------
;; Mouse-motion (sent from enclosing view)
;;
(defun on-mouse-motion (buffer iter)
  (with-slots ((old pointercons)) buffer
    (setf old
	  (contexts-update buffer iter old
			   #'-con-mouse-exit
			   #'-con-mouse-enter))))





(defmethod -con-enter ((ctx ctx) i)
  (format t "conbuf.lisp:-CON-ENTRY ~A ~A~&" ctx i))
(defmethod -con-exit ((ctx ctx) i)
  (format t "conbuf.lisp:-CON-EXIT ~A ~A~&" ctx i))
(defmethod -con-mouse-enter ((ctx ctx) i)
  (format t "conbuf.lisp:-CON-MOUSE-ENTRY ~A ~A~&" ctx i))
(defmethod -con-mouse-exit ((ctx ctx) i)
  (format t "conbuf.lisp:-CON-MOUSE-EXIT ~A ~A~&" ctx i))
