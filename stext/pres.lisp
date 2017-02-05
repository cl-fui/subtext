(in-package :stext)
;;==============================================================================
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Presentations
;;
;; Currently, a presentation consists of: a tag to indicate the range and type
;; of a presentation, and a mark to indicate the instance of a presentation.
;;
;; Note: the same tag is used for all instances of a presentation of that type.
;;
;; FAQ:
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: Why do we need a tags to mark presentations?
;; A: Tag maintain the start and end positions within the buffer, as well as
;;    visually marking the presentation.
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: Why can't we use tags as presentations by subclassing?
;; A: The _same tag_ is used for all presentations of that type.  We would need
;;    to create and add to the table a tag for every presentation instance!
;;    Tags are rather large, and slow down the system exponentially as added.
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: Why do we create a tag class for every presentation class, even though
;;    we create and use only one tag for all presentation instances?  There
;;    is only one instance of that class... Why not
;;    just create a single tag class, and create tags of that class with
;;    different arguments to change colors etc at make-instance time?
;; A: Don't forget that tags coalesce when overlapped.  So creating sub-
;;    presentations is impossible if both share the same tag class.
;;
;; create presentation classes after the buffer exists
;;

;;------------------------------------------------------------------------------
;; Create a tag-derived class that will be a base class for all presentation
;; tags.  This way we can tell if it's just a tag, or a presentation tag!
;; All presentation tags contain a symbol representing the type of the mark
;; used with that presentation!
(defclass ptag-base (gtk-text-tag)
  ((mark-type :accessor mark-type :initform nil :initarg :mark-type))
  (:metaclass gobject-class))

;;------------------------------------------------------------------------------
;; All presentation marks are instances of pmark.  They ref the presentation
(defclass pmark (gtk-text-mark)
  ((pres :accessor pres :initarg :pres))
   ;for verification
  (:metaclass gobject-class))

(defmethod print-object ((mark pmark) out)
   (print-unreadable-object (mark out :type t)
     (format out "*~s ~A ~A" (gtm-name mark) (pres mark) (tag (pres mark)) )))
;;------------------------------------------------------------------------------
;; All presentations are derived from this one.  Note that derived classes
;; all introduce a tag slot
(defclass pres ()
  ()
)


;;------------------------------------------------------------------------------
;; presentation magic
;;
;; A macro to define a presentation class.
;; - create a tag for all presentations of this type
;; - attach tag to buffer (buffer must already exist!
;; - create a mark-derived class
;; This is complicated by the fact
;; that we also need a tag to represent presentations, but tags must
;; be attached to a buffer via tag-table... Therefore:
;;
(defmacro defpres (buffer classname direct-superclass &key (slots nil) (tag nil) )
  "Create a presentation class for an existing buffer, with a new tag 
initialized with :tag (..) arguments, and containing optional slots"
  (let ((tagsym (intern  (concatenate 'string "TAG-" (symbol-name classname))))
	(slot-descriptors
	 (loop for slotsym in slots
	      for slot-initarg = (intern (symbol-name slotsym) 'keyword)
	    collect `(,slotsym :accessor ,slotsym :initarg ,slot-initarg))))
    `(progn
       (defclass ,tagsym (ptag-base) () (:metaclass gobject-class))
       ;; now that tag class is defined
       (let ((mytag (make-instance ',tagsym
				 :mark-type ',classname ,@tag)))
	 (gttt-add (gtb-tag-table ,buffer) mytag)
	 (defclass ,classname ,direct-superclass
	   (,@slot-descriptors 
	    (tag :accessor tag :initform mytag)))
	 ))))



;;------------------------------------------------------------------------------
;; This is a mark inserted by a promise with a presentation. 
(defun pres-mark (buffer iter pres)
  "mark presentation at iter"
;;  (format t "ADDING MARK: ~A ~A~&" pres (type-of pres))
  (gtb-add-mark buffer (make-instance 'pmark :pres pres) iter))

(defun gti-pmarks (iter)
    (remove-if-not (lambda (val) (eq (type-of val) 'pmark)) (gti-marks iter)))

(defun pres-mark-for-ptag (iter ptag)
  (loop for mark in (gti-marks iter)
     when (and (typep mark 'pmark); only care about presentations
	       (eq ptag (tag (pres mark)))) do
       (return mark)))

(defun pres-bounds (stream at ptag)
  "assuming ptag really is here..."
   (with-slots (iter iter1) stream
    (%gtb-get-iter-at-offset stream iter at)
    (%gtb-get-iter-at-offset stream iter1 at)

    (prog2
	(or (gti-begins-tag iter ptag)
	    (gti-backward-to-tag-toggle iter ptag))
	(pres-mark-for-ptag iter ptag)
      (or (gti-ends-tag iter1 ptag)
	  (gti-forward-to-tag-toggle iter1 ptag)))))


(defgeneric present (obj stream extra))
