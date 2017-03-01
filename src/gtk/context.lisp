(in-package :subtext)
;;==============================================================================
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; ConTexts
;;
;; Currently, a context consists of: a tag to indicate the range and type
;; of a context, and a mark to indicate the instance of a context.
;;
;; Via some magic below, in addition to user-declared slots, all contexts
;; have a class-allocated slot called 'out', containing the buffer/stream, and
;; 'tag', containing the tag used to mark the context.
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
;; This is a mark inserted by a promise with a context!!!
;;
(defun context-mark (buffer iter ctx)
  "mark context at iter"
;;  (format t "ADDING MARK: ~A ~A~&" ctx (type-of ctx))
  (gtb-add-mark buffer ctx iter;(make-instance 'pmark :ctx ctx) iter
		))
;;
;; Perform a function for each context at xiter.  Fuction
;; is called as (fun ctx), with iters set to range!  Normally returns nil
;; upon processing all contexts.  If loop is exited, ctx that triggered
;; the exit is returned.
(defun do-contexts-at (stream xiter  fun)
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
       when (subtypep (type-of tag) 'ptag-base)  ;only care about ptags!
       unless (progn
		(%gtb-get-iter-at-offset stream iter (gti-offset xiter));iter at ptag
		(gti-begins-tag iter tag))
       collect
	 (progn
	   (gti-backward-to-tag-toggle iter tag);  move back to start
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

(defun %context-bounds (subtext context iter iter1)
  (%gtb-get-iter-at-mark subtext iter context)
  (%gtb-get-iter-at-mark subtext iter1 context)
  (gti-forward-to-tag-toggle iter1 (tag context)))

;; Convenience macro exposes subtext,iter and iter1.

(defmacro with-subtext (stream &body body)
  `(let ((subtext ,stream))
     (with-slots (iter iter1) subtext
       ,@body)))

(defmacro with-context-bounds (context &body body)
  "within a subtext, get bounds of a context into iters"
  `(progn (%context-bounds subtext ,context iter iter1)
	  ,@body))





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




;;------------------------------------------------------------------------------
;; Defining context classes
;;
;; slots can be either slotnames, in which case we make an accessor and an
;; initform, or whatever you want inside ()...
;;
(defmacro defcontext (classname direct-superclass slots   )
  "Define a context class."
  (let* ((name classname)
	 (newslots
	 (loop for slot in slots
	    collect
	      (typecase slot
		(symbol (let ((keyname (intern (string-upcase slot) :keyword)))
			  `(,slot :accessor ,slot :initarg ,keyname )) )
		(t slot)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (prog1
	   (defclass ,name ,direct-superclass
	     (,@newslots
	      (out :accessor out :initform nil :allocation :class)
	      (tag :accessor tag :initform nil :allocation :class))
	     (:metaclass gobject-class))))) )

;;------------------------------------------------------------------------------
;; Keymapping contexts
;;
;; Keymapping is not inherent in the class.  Why?  If the keymap is in a slot,
;; then every instance will have a different keymap.  If the keymap is in the
;; class, all derived classes will have the same keymap.  Each derived class
;; can add its own keymap slot, but that is a little hard to manage.
;;
;; The keymap is created as a KEYMAP-xxx parameter, and a (keymap ..) function
;; specialized on the type is created for dynamic resolution.
;;
(defun make-keymap-name (sym-classname)
  (concatenate 'string "KEYMAP-" (symbol-name sym-classname)))
(defmacro defkeymap (classname)
  (let* ((name classname)
	 (keymap-sym 
	  (intern (make-keymap-name name))))
    `(progn  (defparameter ,keymap-sym nil)
	     (defmethod -con-keyseq (subtext (context ,name) keyseq)
	       (keymap-process ,keymap-sym keyseq subtext context)))))


;; These are simplified macros for binding keyseqs.  Example
;; (bindkeys button ("Mouse-1" (whatever) nil))
(defmacro keys-bind (classname keystr &body body)
  (let* ((sym-keymap-name (find-symbol (make-keymap-name classname))))
    `(keymap-def ,sym-keymap-name (kbd ,keystr)
		 (lambda (subtext context)
		   (declare (ignorable subtext context))
		   ,@body))))


;; Simplified keyseq binding for elis.
(defmacro keys-eli (eli keystr &body body)
  `(keymap-def (keymap ,eli) (kbd ,keystr)
	       (lambda (subtext context)
		 (declare (ignorable subtext context))
		 ,@body)))

;;------------------------------------------------------------------------------
;; All context marks are instances of pmark.  They ref the context
(defclass ctx (gtk-text-mark)
  ()
  (:metaclass gobject-class))
;;------------------------------------------------------------------------------
;; default context processing
;;
(defmethod -con-enter (subtext (ctx ctx) i)
  (format t "conbuf.lisp:-CON-ENTRY ~A ~A~&" ctx i))
(defmethod -con-exit (subtext (ctx ctx) i)
  (format t "conbuf.lisp:-CON-EXIT ~A ~A~&" ctx i))
(defmethod -con-mouse-enter (subtext (ctx ctx) i)
  (format t "conbuf.lisp:-CON-MOUSE-ENTRY ~A ~A~&" ctx i))
(defmethod -con-mouse-exit (subtext (ctx ctx) i)
  (format t "conbuf.lisp:-CON-MOUSE-EXIT ~A ~A~&" ctx i))
(defmethod -con-keyseq (subtext (ctx ctx) keyseq)
  0; which means "not found".  Nil means found and done!
  )
