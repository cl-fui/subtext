(in-package :subtext)
;;==============================================================================
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; ConTexts
;;
;; Currently, a context consists of: a tag to indicate the range and type
;; of a context, and a mark to indicate the instance of a context.
;;
;; Note: the same tag is used for all instances of a context of that type.
;;
;; FAQ:
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: Why do we need a tags to mark contexts?
;; A: Tags maintain the start and end positions within the buffer, as well as
;;    visually marking the context.
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: Why can't we use tags as contexts by subclassing?
;; A: The _same tag_ is used for all contexts of that type.  We would need
;;    to create and add to the table a tag for every context instance!
;;    Tags are rather large, and slow down the system exponentially as added.
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: Why do we create a tag class for every context class, even though
;;    we create and use only one tag for all context instances?  There
;;    is only one instance of that class... Why not
;;    just create a single tag class, and create tags of that class with
;;    different arguments to change colors etc at make-instance time?
;; A: Don't forget that tags coalesce when overlapped.  So creating sub-
;;    contexts is impossible if both share the same tag class.
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Q: Why is there a 'tag' slot in the context classes?
;; A: To clarify, the slot is in the _class_, not instances.  Each context
;;    class holds the single tag that establishes bounds for instances. 
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Order of tags is important: older tags will visually disappear under newer
;; tags in some cases!
;;
;; 

;;------------------------------------------------------------------------------
;; Create a tag-derived class that will be a base class for all context
;; tags.  This way we can tell if it's just a tag, or a context tag!
;; All context tags contain a symbol representing the type of the mark
;; used with that context!
(defclass ptag-base (gtk-text-tag)
  ((mark-type :accessor mark-type :initform nil :initarg :mark-type)
  ;; (desc :accessor desc :initform desc )
   )
  (:metaclass gobject-class))
(defmethod print-object ((tag ptag-base) out)
   (format out "<PTAG for '~A>"  (mark-type tag) ))

;;------------------------------------------------------------------------------
;; All context marks are instances of pmark.  They ref the context
(defclass ctx (gtk-text-mark)
  ()
  (:metaclass gobject-class))

;; default key processing: null keymap...
(defmethod keymap ((ctx ctx))
  nil)


(defmethod print-object ((mark ctx) out1)
  (print-unreadable-object (mark out1 :type t)
    (with-slots (out) mark
 ;     (format t "OUT: ~A" out)
					;
      ;
      
      )
))

;;------------------------------------------------------------------------------
;; This is a mark inserted by a promise with a context!!!
;;
(defun context-mark (buffer iter ctx)
  "mark context at iter"
;;  (format t "ADDING MARK: ~A ~A~&" ctx (type-of ctx))
  (gtb-add-mark buffer ctx iter;(make-instance 'pmark :ctx ctx) iter
		))

(defun gti-pmarks (iter)
    (remove-if-not (lambda (val) (eq (type-of val) 'pmark)) (gti-marks iter)))

(defun context-iters (ctx)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-slots (out tag) ctx
    (with-slots (iter iter1) out
      (%gtb-get-iter-at-mark out iter ctx)
      (%gtb-get-iter-at-mark out iter1 ctx)
      (gti-forward-to-tag-toggle iter1 tag))))


(defun context-tag-bounds (stream at ptag)
  "set iters to tag bounds of a tag; at is inside it"
   (with-slots (iter iter1) stream
    (%gtb-get-iter-at-offset stream iter at)
    (%gtb-get-iter-at-offset stream iter1 at)
    (prog2
	;(or (gti-begins-tag iter ptag))
	(gti-backward-to-tag-toggle iter ptag)
;;	(ctx-mark-for-ptag iter ptag)
      (or (gti-ends-tag iter1 ptag)
	  (gti-forward-to-tag-toggle iter1 ptag)))))


;;
;; Perform a function for each context at xiter.  Fuction
;; is called as (fun ctx), with iters set to range!  Normally returns nil
;; upon processing all contexts.  If loop is exited, ctx that triggered
;; the exit is returned.
(defun do-contexts-at (stream xiter fun)
  "for every context at xiter, call (fun ctx).  If it returns t, stop"
  (with-slots (iter iter1) stream
    (loop for tag in (reverse (gti-tags xiter));TODO: is reverse good enough?
       when (subtypep (type-of tag) 'ptag-base) do ;only care about ptags!
       ;; set iter to start of tag
	 (%gtb-get-iter-at-offset stream iter (gti-offset xiter));iter at ptag
					;(format t "~&BEGINS-TAG ~A: ~A~&" tag (gti-begins-tag iter tag))
	 (unless (gti-begins-tag iter tag) 	; start of tag is premature...
	   (gti-backward-to-tag-toggle iter tag); or move back to start
	   (let ((ctx (find (mark-type tag) (gti-marks iter); find matching mark
			     :test (lambda (key item)  (eq key (type-of item))))))
	     (%gtb-get-iter-at-offset stream iter1 (gti-offset xiter));iter1
	     (or (gti-ends-tag iter1 tag) ;either we end a tag
		 (gti-forward-to-tag-toggle iter1 tag)); or forward to end
	     (let ((result (funcall fun ctx))) ;call function with iterators set
	       (return result)))))))

(defun do-contexts-at-off (stream off fun)
  (do-contexts-at stream (gtb-get-iter-at-offset stream off) fun))

;;------------------------------------------------------------------------------
;; Contexts at iterator
;;
(defun contexts-at (stream xiter)
  "Return a list of every context that is bisected by iter"
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

(defun contexts-at-off (stream off)
  (contexts-at stream (gtb-get-iter-at-offset stream off)))
;;------------------------------------------------------------------------------
(defun context-bounds(stream ctx)
  (with-slots (iter iter1) stream
    (%gtb-get-iter-at-mark stream iter ctx)
    (%gtb-get-iter-at-mark stream iter1 ctx)
    ;; We know that tag begins here, and what kind of tag...
    (gti-forward-to-tag-toggle iter1 (tag ctx))))




;;------------------------------------------------------------------------------
;; context magic
;;
;; 
;;

;; Call this during buffer initialization, to create a matching tag
(defmacro context-tag (buffer class tag-options &optional (ctx-options nil))
  "in this buffer, associate a context with a realized tag."
  (let ((buf (gensym))
	(ctx (gensym))
	(tag (gensym))
	(cls (gensym)))
    `(let* ((,buf ,buffer)
	    (,cls ',class) ;evaluate once
	    (,ctx (make-instance ,cls ,@ctx-options))
	    (,tag  (make-instance 'ptag-base ,@tag-options :mark-type ,cls)))
       (setf (out ,ctx) ,buf
	     (tag  ,ctx) ,tag)
       (gttt-add (gtb-tag-table ,buffer) ,tag))))



(defmacro defkeymap (classname)
  (let ((keymap-sym 
	 (intern (concatenate 'string "KEYMAP-" (symbol-name classname)))))
    `(progn  (defparameter ,keymap-sym nil)
	     (defmethod keymap ((context ,classname)) ,keymap-sym)))

)
;;------------------------------------------------------------------------------
;; Defining context classes
;;
;; slots can be either slotnames, in which case we make an accessor and an
;; initform, or whatever you want inside ()...
;;
(defmacro defcontext (classname direct-superclass slots &key (keymap nil)  )
  "Define a context class."
  (let* ((name classname)
	 (newslots
	 (loop for slot in slots
	    collect
	      (typecase slot
		(symbol (let ((keyname (intern (string-upcase slot) :keyword)))
			  `(,slot :accessor ,slot :initarg ,keyname )) )
		(t slot))))
	(keymap-sym 
	 (intern (concatenate 'string "KEYMAP-" (symbol-name name))))
	(keystuff
	 (and keymap
	      `(progn  (defparameter ,keymap-sym nil)
		       (defmethod keymap ((context ,name)) ,keymap-sym)))))
    
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (prog1
	   (defclass ,name ,direct-superclass
	     (,@newslots
	      (out :accessor out :initform nil :allocation :class)
	      (tag :accessor tag :initform nil :allocation :class))
	     (:metaclass gobject-class))
	 ,keystuff)))
  )

;;------------------------------------------------------------------------------
;; Keymapping contexts
;;
;; Keymapping is not inherent in the class (for some good reasons), but is
;; an addon to any context class.  Once initiated, all instances of that class
;; share a common keymap.
;;
;; The keymap is created as a KEYMAP-xxx parameter, and a (keymap ..) function
;; specialized on the type is created for dynamic resolution.
;;
(defmacro keymapped-context (classname)
  (let* ((name classname)
	 (keymap-sym ;classname is 'name, which comes in as (quote name)
	  (intern (concatenate 'string "KEYMAP-" (symbol-name name)))))
    `(progn (defparameter ,keymap-sym nil)
	    (defmethod keymap ((context ,name)) ,keymap-sym))) )



