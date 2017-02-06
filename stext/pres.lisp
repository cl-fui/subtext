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
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: Why is there a 'tag' slot in the presentation classes?
;; A: To clarify, the slot is in the _class_, not instances.  Each presentation
;;    class holds the single tag that establishes bounds for instances. The
;;    other slots in the class, are buffer and tagdesc.
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: What's the deal with tagdesc?  How is it different from tag?
;; A: The presentation is defined before the buffer is instantiated.  However
;;    it relies on tags which need to be connected to a live buffer.  So we
;;    keep track of what kind of a tag the presentation needs by storing the
;;    tag instantiation parameters (the keyword list sent to make-instance
;;    'ptag...) in tagdesc, in the presentation _class_.  Later, after the
;;    buffer is established, the tag is actually created using tagdesc, and
;;    attached to the presentation _class_ .  When we work with presentations
;;    they will have access to the tag in the buffer.
;;
;;
;; create presentation classes after the buffer exists
;;

;;------------------------------------------------------------------------------
;; Create a tag-derived class that will be a base class for all presentation
;; tags.  This way we can tell if it's just a tag, or a presentation tag!
;; All presentation tags contain a symbol representing the type of the mark
;; used with that presentation!
(defclass ptag-base (gtk-text-tag)
  ((mark-type :accessor mark-type :initform nil :initarg :mark-type)
  ;; (desc :accessor desc :initform desc )
   )
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
;; all introduce a tag slot in the derived class (not instance!)
(defclass pres ()
  ;; In the wild, there are class-allocated slots for:
  ;; tagdesc: a list to pass to (create-instance 'tag ...)
  ())

;;------------------------------------------------------------------------------
;; A prototype of a presentation.  We can create it early on, and realize it
;; later, when the buffer is active.
(defstruct proto-pres name super slotdesc tagdesc)
(defun make-pres (name super slotdesc tagdesc)
       (make-proto-pres :name name :super super :slotdesc slotdesc))

(defun pbuf-pres-classes (buffer list-of-symbols)
  "connect the presentation classes to this buffer"
  (mapc #'(lambda (sym) (pres-in-buffer buffer sym))
	 list-of-symbols))
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



;;------------------------------------------------------------------------------
;; presentation magic
;;
;; 
;;
(defmacro defpres (classname direct-superclass slots )
  "Create a presentation class and a tag class."
  `(defclass ,classname ,direct-superclass
     (,@slots
      (out :accessor out :initform nil :allocation :class)
      (tag :accessor tag :initform nil :allocation :class))) )

(defmacro pres-tag (buffer class tag-options)
  (let ((buf (gensym))
	(pres (gensym))
	(tag (gensym)))
    `(let ((,buf ,buffer)
	   (,pres (make-instance ',class))
	   (,tag  (make-instance 'ptag-base ,@tag-options)))
       (setf (out ,pres) ,buf
	     (tag    ,pres) ,tag)
       (gttt-add (gtb-tag-table ,buffer) ,tag))))
