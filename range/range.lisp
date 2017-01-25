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


(define-condition range-error (error)
  ()
  (:documentation "Error in range system.  You are hosed."))


(defclass ranges ()
  ((root      :initform (make-range :data t)  :accessor root) ;; root node
   (ht        :initform (make-hash-table :test 'eq)
	      :accessor ht))     ;; register obj->node
)

(defstruct (range
	     (:conc-name nil)
	     (:print-function (lambda (o s k) (declare (ignore k))
					   (format s "<~A ~A>"
						   (width o)
						   (data o)
						   ))))
  
  ;;
  (width 0   :type fixnum)
  (data  nil :type t)
  (l     nil :type (or null range))
  (dad   nil :type (or null range))
  (child nil :type (or null range))
  )

(defun make-top ()
  (make-range :data t))
;;
;; NEW
;;
;; This only works at the very end - it is the only zero-width range we
;; can ever find!
;;
;; Special case: if the child is a null-node, just take posession of it.
(defun new (ranges dad &optional (data nil) (register nil))
  "Insert a new range in parent's right side. Return it"
  (with-slots ((dad-child child)) dad
    (let ((range
	   (if (and dad-child ;special case - assume identity of null child
		    (zerop (width dad-child))
		    (null  (data  dad-child)))
	       (progn (setf (data dad-child) data))
	       (setf dad-child
		     (make-range :dad dad :data data
				 :l dad-child)))))
      (and register
	   data			; do not insert null data!
	   (setf (gethash data (ht ranges)) range))
      range)))

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

(defun widen (range by)
  (when range
    (incf (width range) by)
    (widen (dad range) by)
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
(defun prim (range off)
  (with-slots (width child l) range
    (if (>= width off) ;if width>off, it is inside here
	(if child; if there are children, 
	    (prim child off); maybe?
	    (values range off)); no children means this is it.
	(if l; width<= offset, more to go.
	    (prim l (- off width)); try next to the left.
	    (values (dad range) off); no more? dad is it.
	    ))))

(defun at_ (tree off)
  "Return the range that encloses off"
  (let* ((width (width tree))
	 (rem (- width off))); because from right!
    (if (child tree)
	(prim (child tree) rem)
	(values tree rem))))

(defun at (ranges off)
  (at_ (root ranges) off))

(defun find (ranges item)
  (gethash item (ht ranges)))

(defun kids (dad)
  (loop for r = (child dad) then (l r)
       while r
       collecting r))

#|| Create a sub-range at offset, with width 1
-find range, make sure it encloses us.
-figure how much is left over on R, for padding.
-create a pad child and a real one.

||#



(defparameter *a* nil)
(defparameter *b* nil)
(defparameter *tab* (make-instance 'ranges))
(setf *a* (widen (new *tab* (root *tab*) 'a) 10))
(setf *b* (widen(new *tab* (root *tab*) 'b) 20))


