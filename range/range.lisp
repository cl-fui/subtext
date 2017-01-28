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


(defstruct (range
	     (:constructor make (&key (width 0) (dad nil) (l nil)))
	     (:conc-name nil)
	     (:print-function (lambda (o s k) (declare (ignore k))
				      (format s "<~C~A ~A >"
					      (if (child o)#\* #\space)
					      (type-of o)
					      (width o)))))
  (width 0   :type fixnum)
  (l     nil :type (or null range))
  (dad   nil :type (or null range))
  (child nil :type (or null range)))

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
(defun prim (range off r)
  (with-slots (width child l) range
    (if (>= width off) ;if width>off, it is inside here
	(if child; if there are children, 
	    (prim child off nil); maybe?
	    (values range off r))   ; no children means this is it.
	(if l; width<= offset, more to go.
	    (prim l (- off width) range); try next to the left.
	    nil; this cannot happen...
	    ))))

;; This routine is used to insert ranges below.  Here we find the encloser,
;; as well as the right node so we can fix its l pointer or nil if we are first
(defun at (root from-left)
  "Find the range that encloses the offset, and return
it, rem and right node"
  (let ((off (- (width root) from-left ))) ; because from right!
    (prim root off nil)))



(defun kids (dad)
  (loop for r = (child dad) then (l r)
       while r
       collecting r))

#|| Create a sub-range at offset, with width 1
-find range, make sure it encloses us.
-figure how much is left over on R, for padding.
-create a pad child and a real one.

||#


;; Sub-of creates a subrange of a dad range.
;; range is set with a real dad and width.
(defun subrange (range end root)
  (with-slots (dad width child l) range
    ;; The parent range must have real children for this to work...
    (unless (child dad) (setf (child dad) (make :dad  dad :width (width dad))))
    ;; now, get the range at our position and make sure that it is nil and
    ;; completely encloses us.
  
    (multiple-value-bind (encloser off rnode) (at root end)
      (when (or (null encloser)
		(not (eq 'range (type-of encloser)))
		(> width (- (width encloser) off))
		(not (eq (dad encloser) dad)))
	(error "Cannot insert a subrange: ~A at ~A; into ~A at ~A" range end encloser off))
      (format t "encloser ~A ~A~&" encloser off)
      (let ((remaining (-(width encloser) width)))
	(format t "remaining ~A~&" remaining)
	(if (plusp off)			
	    (progn; convert encloser to filler
	      (setf (width encloser) off 
		    l (l encloser) ;we follow!
		    (l encloser) range) 
	      (decf remaining off)
	      (when (plusp remaining)
		;(print "OK")
		(if (eq 'range (type-of l)) ;if node to our left is pad
		    (incf (width l) remaining) ;simply widen it
		    ;; otherwise, insert pad node to our left
		    ;(progn (print "ADDING ON") (setf l (make :width remaining :dad dad :l l)))
		    )))
	    (progn; insert us and and fill with encloser
	      (setf l encloser)
	      (format t "WIDTH OF ENCLOSER ~A~&" (width encloser))
	      (if rnode
		  (setf (l rnode) range)
		  (setf (child dad) range))
	      (decf (width encloser) width)
	      (format t "WIDTH OF ENCLOSER NOW ~A~&" (width encloser))))
	)))
  range)



(defun sub (range endoff)
  (with-slots (dad width child l) range
    (unless (child dad) (setf (child dad) (make :dad  dad :width (width dad))))
    (multiple-value-bind (encloser off rnode) (at dad endoff)
       (when (or (not (eq 'range (type-of encloser)))
		(> width (- (width dad) off))
		(not (eq (dad encloser) dad)))
	(error "Cannot insert a subrange: ~A at ~A; into ~A at ~A" range endoff encloser off))
           (let ((remaining (-(width encloser) width)))
	(if (plusp off)			
	    (progn; convert encloser to filler
	      (setf (width encloser) off 
		    l (l encloser) ;we follow!
		    (l encloser) range) 
	      (decf remaining off)
	      (when (plusp remaining)
		(if (eq 'range (type-of l)) ;if node to our left is pad
		    (incf (width l) remaining) ;simply widen it
		    ;; otherwise, insert pad node to our left
		   ; (progn (print "ADDING ON") (setf l (make :width remaining :dad dad :l l)))
		    )))
	    (progn; insert us and and fill with encloser
	      (setf l encloser)
	      (if rnode
		  (setf (l rnode) range)
		  (setf (child dad) range))
	      (decf (width encloser) width))))))
  range)

(defparameter *a* nil)
(defparameter *b* nil)
(defparameter *tab* (make))
(setf *a* (widen (new-in *tab* nil) 100))
;;(setf *b* (subrange (make :width 12 :dad *a*) 20 *tab* ))
(setf *b* (sub (make :width 12 :dad *a*) 20 ))
(sub (make :width 3 :dad *b*) 13) 


