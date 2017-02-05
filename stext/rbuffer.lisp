(in-package :stext)
;;;=============================================================================
;;; rbuffer - a gtk-text-buffer 
;;;
;;OK (ql:quickload :stext)(in-package :stext)
(defclass rbuffer (termstream)
  ()
(:metaclass gobject-class))

;;------------------------------------------------------------------------------
;; presentation magic
;;
;; A macro to define a presentation class.  This is complicated by the fact
;; that we also need a tag to represent presentations, but tags must
;; be attached to a buffer via tag-table... Therefore:
;;
;; create presentation classes after the buffer exists
;; 
(defmacro defpres (classname buffer tagarg &rest slots )
  (let ((tagsym (intern  (concatenate 'string "TAG-" (symbol-name classname)))))
   
    `(progn
       (defclass ,tagsym (ptag)
	 ()
	 (:metaclass gobject-class))
       ;; now that tag class is defined
       (let ((tag (make-instance ',tagsym ,@tagarg))
	     )
	 (gttt-add (gtb-tag-table ,buffer) tag)
	 (defclass ,classname (pres)
	   (,@slots
	    (tag :accessor :tag :initform tag)))))))
