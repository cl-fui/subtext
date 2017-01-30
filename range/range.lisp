(in-package :range)
#||
We maintain a right-to-left list of widths in the buffer.  Since most of the activity in a text buffer happens at the end, this should help.

;; If things get too slow, an index may be introduced
 (:print-function (lambda (o s k) (declare (ignore k))
					   (format s "<~A ~A ~A>"
						   (range-width o)
						   (range-data o)
						   (range-child o))))
||#


(define-condition range-error (simple-error)
  ((message :initarg :message :reader message))
  )


(defclass range ()
  ((width :accessor width :initform 0   :initarg :width :type fixnum)
   (l     :accessor l     :initform nil :initarg :l     :type range)
   (dad   :accessor dad   :initform nil :initarg :dad   :type range)
   (child :accessor child :initform nil :initarg :child :type range))
  )
(declaim (inline rangep))
(defun rangep (obj)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (eq 'range (type-of obj)))

(defmethod print-object ((obj range) out)
  (with-slots (width child) obj
    (format out "<~C~A ~A>" (if child #\* #\ )
	    (type-of obj) width)))

(defmacro make (&rest rest)
  `(make-instance 'range ,@rest))
;;
;; NEW
;;
;; This only works at the very end - it is the only zero-width range we
;; can ever find!
;;
;; Special case: if the child is a null-node, just take posession of it.
(defun new-in (dad range)
  "Insert a new range in parent's right side. Return it"
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type range dad)
	   (type (or range null) range)
	   )
  (with-slots ((dad-child child)) dad
    (if range
	(setf (dad range) dad
	      (l   range) dad-child
	      dad-child range)
	(setf dad-child
	      (make :dad dad
		    :l   dad-child)))))

(defun end (range)
  "find the absolute end position of the range"
  (declare (type range range))
  (let ((dad (dad range)))
    (if dad
	(- (end dad)
	   (loop for r = (child dad) then (l r)
	      until (eq r range)
	      summing (width r) into total
	      finally (return total)))
	(width range))))
(defun bounds (range)
  "return 2 values for start and end of range"
  (let ((end (end range)))
    (values (- end (width range)) end)))

(defun widen-prim (range by)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when range
    (incf (the fixnum (width range))
	  (the fixnum by))
    (widen-prim (dad range) by)
    range))


;; This is the only problem area...
(defun narrow (range by)
  (when range
    (if (>= (width range) by) ;TODO: should not be allowed, but at end...
	(decf (width range) by)
	(error 'error "RANGE ~A too small to delete ~A chars" range by))
    (narrow (dad range) by)
    range)
  )
;;==============================================================================
;; This routine is used to insert ranges below.  Here we find the encloser,
;; as well as the right node so we can fix its l pointer or nil if we are first
(defun at-prim (range off r)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (with-slots (width child l) range
      (declare (type fixnum width))
    (if (>= (the fixnum width) (the fixnum off)) ;if width>off, it is inside
	(if child; if there are children, 
	    (at-prim child off nil); maybe?
	    (values range off r))   ; no children means this is it.
	(if l; width<= offset, more to go.
	    (at-prim l (the fixnum (- (the fixnum off)
				   (the fixnum width)))
		  range) ; try next to the left.
	    nil; this cannot happen...
	    ))))


(defun at (root from-left)
  "Find the range that encloses the offset, and return
it, rem and right node."
  (let ((off (- (width root) from-left ))) ; because from right!
    ;;(format t "~&AT ~A off~&" off)
    (at-prim root off nil)))

(defun uat (root from-left)
  "find a good range, that is not a range:range (often used as a spacer) 
but a derived type."
  (loop for val = (at root from-left)
     then (dad val)
     while (eq (type-of val) 'range)
     while val
     finally (return val)  ))


(defun widen (root at by)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((off (- (the fixnum (width root))
		(the fixnum at) ))) ; because from right!
    ;;(format t "~&AT ~A off~&" off)
    (if (zerop off)
	(widen-baby root by)
	(widen-prim (at-prim root (the fixnum off) nil) by))))

(defun kids (dad)
  (loop for r = (child dad) then (l r)
       while r
       collecting r))

#|| Create a sub-range at offset, with width 1
-find range, make sure it encloses us.
-figure how much is left over on R, for padding.
-create a pad child and a real one.

||#


;; We are trying to create a subrange in a parent range.
;; If the parent has no children, we will start by creating a full-
;; width nil child range, and subdivide that.
;; If it has children, we use at to find a child range that encloses us,
;; and if it is nil, split it up.  If it's not nil, we create a child
;; as above, and subdivide it.
;;
;; Error conditions include - request too wide

(defun sub (range endoff)
  (with-slots (l width dad) range
    (unless (child dad)
      (setf (child dad) (make :dad  dad :width (width dad))))
    (multiple-value-bind (encloser off rnode) (at dad endoff)
      (if (> (+ width off) (width encloser))
	  (error "RANGE:SUB range ~A does not fit into the enclosing range ~A at ~A" range encloser off))
      (unless (eq 'range (type-of encloser))
	(when (child encloser)
	  (error "RANGE:SUB found an enclosing range ~A, but it already has subranges." encloser))
	(setf encloser (setf (child encloser)
			     (make :dad  encloser :width (width encloser)))))
      (setf dad (dad encloser))
      (let ((remaining (- (width encloser) width)))
	;;(format t "REMAINING ~A  OFF ~A RNODE ~A~&" remaining off rnode)
	(if (plusp off) ;front pad?
	    (progn; convert encloser to filler
	      (setf (width encloser) off 
		    l (l encloser) ;we follow!
		    (l encloser) range) 
	      (decf remaining off)
	      (when (plusp remaining)
		;;(print "REMAINING")
		(if (eq 'range (type-of l)) ;if node to our left is pad
		    (incf (width l) remaining) ;simply widen it
		    ;; otherwise, insert pad node to our left 
		    
		    (progn ;;(print "ADDINGON")
			   (setf l (make :width remaining :dad (dad range) :l l))))))
	    ;; no offset - we come first, then, encloser will pad us
	    (progn
	      (if rnode
		  (setf (l rnode) range)
		  (setf (child dad) range))
	      (if (zerop remaining) ;if we fill the entire space,
		  (setf l (l encloser)) ;bypass encloser and release it
		  (progn (decf (width encloser) width) ;otherwise, adjust width
			 (setf l encloser))))))))
  range)

;; We _have_ to differentiate between appending new ranges and manipulating old ones, sadly...
(defun widen-baby (root by)
  "Widen the first (rightmost) range. Make sure we are outputting into a clean null node."
  (when (dad root) (error "RANGE:APPEND-TERM requires a root node, not ~A!" root))
  ;;acceptable situations
  (with-slots (child width) root
    (when child
	(if (rangep child)
	    (incf (width child) by)
	    (setf child (make :dad root :l child :width by))))
    (incf width by)))



(defparameter *a* nil)
(defparameter *b* nil)
(defparameter *tab* (make))
;;(setf *b* (conjoin (make :dad *tab*)))
;;(widen (at *tab* 0) 5)
;;(conjoin (make :dad *tab*))


