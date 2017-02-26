(in-package :subtext)
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
;; A: Tags maintain the start and end positions within the buffer, as well as
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
;;    class holds the single tag that establishes bounds for instances. 
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Order of tags is important: older tags will visually disappear under newer
;; tags in some cases!
;;
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
(defmethod print-object ((tag ptag-base) out)
   (format out "<PTAG for '~A>"  (mark-type tag) ))

;;------------------------------------------------------------------------------
;; All presentation marks are instances of pmark.  They ref the presentation
(defclass pres (gtk-text-mark)
  (;(pres :accessor pres :initarg :pres)
   )
   ;for verification
  (:metaclass gobject-class))

;; default key processing: null keymap...
(defmethod keymap ((pres pres))
  nil)


(defmethod print-object ((mark pres) out1)
  (print-unreadable-object (mark out1 :type t)
    (with-slots (out) mark
 ;     (format t "OUT: ~A" out)
					;
      ;
      
      )
))

;;------------------------------------------------------------------------------
;; This is a mark inserted by a promise with a presentation!!!
;;
(defun pres-mark (buffer iter pres)
  "mark presentation at iter"
;;  (format t "ADDING MARK: ~A ~A~&" pres (type-of pres))
  (gtb-add-mark buffer pres iter;(make-instance 'pmark :pres pres) iter
		))

(defun gti-pmarks (iter)
    (remove-if-not (lambda (val) (eq (type-of val) 'pmark)) (gti-marks iter)))

(defun pres-iters (pres)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-slots (out tag) pres
    (with-slots (iter iter1) out
      (%gtb-get-iter-at-mark out iter pres)
      (%gtb-get-iter-at-mark out iter1 pres)
      (gti-forward-to-tag-toggle iter1 tag))))


(defun pres-tag-bounds (stream at ptag)
  "set iters to tag bounds of a tag; at is inside it"
   (with-slots (iter iter1) stream
    (%gtb-get-iter-at-offset stream iter at)
    (%gtb-get-iter-at-offset stream iter1 at)

    (prog2
	(or (gti-begins-tag iter ptag)
	    (gti-backward-to-tag-toggle iter ptag))
;;	(pres-mark-for-ptag iter ptag)
      (or (gti-ends-tag iter1 ptag)
	  (gti-forward-to-tag-toggle iter1 ptag)))))
;; at the start of ptag

(defun pres-mark-for-ptag (iter ptag)
  (loop for mark in (gti-marks iter)
     when (and (typep mark 'pmark); only care about presentations
	       (eq ptag (tag (pres mark)))) do
       (return mark)))

;;
;; Perform a function for each presentation at xiter.  Fuction
;; is called as (fun pres), with iters set to range!  Normally returns nil
;; upon processing all presentations.  If loop is exited, pres that triggered
;; the exit is returned.
(defun do-pres-at (stream xiter fun)
  "for every presentation at xiter, call (fun pres).  If it returns t, stop"
  (with-slots (iter iter1) stream
    (loop for tag in (reverse (gti-tags xiter));TODO: is reverse good enough?
       when (subtypep (type-of tag) 'ptag-base) do ;only care about ptags!
       ;; set iter to start of tag
	 (%gtb-get-iter-at-offset stream iter (gti-offset xiter));iter at ptag
					;(format t "~&BEGINS-TAG ~A: ~A~&" tag (gti-begins-tag iter tag))
	 (unless (gti-begins-tag iter tag) 	; start of tag is premature...
	   (gti-backward-to-tag-toggle iter tag); or move back to start
	   (let ((pres (find (mark-type tag) (gti-marks iter); find matching mark
			     :test (lambda (key item)  (eq key (type-of item))))))
	     (%gtb-get-iter-at-offset stream iter1 (gti-offset xiter));iter1
	     (or (gti-ends-tag iter1 tag) ;either we end a tag
		 (gti-forward-to-tag-toggle iter1 tag)); or forward to end
	     (let ((result (funcall fun pres))) ;call function with iterators set
	       (return result)))))))

(defun do-pres-at-off (stream off fun)
  (do-pres-at stream (gtb-get-iter-at-offset stream off) fun))

;;------------------------------------------------------------------------------
;; Presentations at iterator
;;
(defun presentations-at (stream xiter)
  "Return a list of every presentation that is bisected by iter"
  (with-slots (iter iter1) stream
    (loop for tag in (reverse (gti-tags xiter));TODO: is reverse good enough?
       when (subtypep (type-of tag) 'ptag-base) ;only care about ptags!
       ;; set iter to start of tag
       collect
	 (progn
	   (%gtb-get-iter-at-offset stream iter (gti-offset xiter));iter at ptag
	   (or ;(gti-begins-tag iter tag); either we start a tag
	       (gti-backward-to-tag-toggle iter tag)); or move back to start
	   (find (mark-type tag) (gti-marks iter); find matching mark
		       :test (lambda (key item)  (eq key (type-of item))))))))

(defun presentations-at-off (stream off)
  (presentations-at stream (gtb-get-iter-at-offset stream off)))
;;------------------------------------------------------------------------------
(defun pres-bounds(stream pres)
  (with-slots (iter iter1) stream
    (%gtb-get-iter-at-mark stream iter pres)
    (%gtb-get-iter-at-mark stream iter1 pres)
    ;; We know that tag begins here, and what kind of tag...
    (gti-forward-to-tag-toggle iter1 (tag pres))))




;;------------------------------------------------------------------------------
;; presentation magic
;;
;; 
;;

;; Call this during buffer initialization, to create a matching tag
(defmacro pres-tag (buffer class tag-options &optional (pres-options nil))
  "in this buffer, associate a presentation with a realized tag."
  (let ((buf (gensym))
	(pres (gensym))
	(tag (gensym)))
    `(let ((,buf ,buffer)
	   (,pres (make-instance ',class ,@pres-options))
	   (,tag  (make-instance 'ptag-base ,@tag-options :mark-type ',class)))
       (setf (out ,pres) ,buf
	     (tag    ,pres) ,tag)
       (gttt-add (gtb-tag-table ,buffer) ,tag))))

;;------------------------------------------------------------------------------
;; Defining presentation classes
;;
;; slots can be either slotnames, in which case we make an accessor and an
;; initform, or whatever you want inside ()...
;;
(defmacro defpres (classname direct-superclass slots )
  "Define a presentation class."
  (let ((newslots
	 (loop for slot in slots
	    collect
	      (typecase slot
		(symbol (let ((keyname (intern (string-upcase slot) :keyword)))
			  `(,slot :accessor ,slot :initarg ,keyname )) )
		(t slot)))))
    `(defclass ,classname ,direct-superclass
       (,@newslots
	(out :accessor out :initform nil :allocation :class)
	(tag :accessor tag :initform nil :allocation :class))
       (:metaclass gobject-class)))
  )

;;------------------------------------------------------------------------------
;; Keymapping subtexts
;; A little more complicated, since we want a keymap for a class of subtexts.
;; Unfortunately, :allocation :class does not work, since derived classes wind
;; up sharing the same slot!
;;
;; So we have to create a unique keymap, and an accessor specialized on the
;; subtext type.
;;
(defmacro defst (classname direct-superclass slots )
  "Define a presentation class."
  (let ((newslots
	 (loop for slot in slots
	    collect
	      (typecase slot
		(symbol (let ((keyname (intern (string-upcase slot) :keyword)))
			  `(,slot :accessor ,slot :initarg ,keyname )) )
		(t slot)))))
    `(defclass ,classname ,direct-superclass
       (,@newslots
	(out :accessor out :initform nil :allocation :class)
	(tag :accessor tag :initform nil :allocation :class))
       (:metaclass gobject-class)))
  )
(defmacro defstk(classname direct-superclass slots)
  (let ((keymap-sym ;classname is 'name, which comes in as (quote name)
	 (intern (concatenate 'string "KEYMAP-" (symbol-name classname)))))
    `(progn (defst ,classname ,direct-superclass ,slots)
	    (defparameter ,keymap-sym nil)
	    (defmethod keymap ((subtext ,classname)) ,keymap-sym)
	    ))
  )



