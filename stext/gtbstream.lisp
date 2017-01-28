(in-package :stext)
;;------------------------------------------------------------------------------
;; gtbstream
;;
;; A thin layer around gtk-text-buffer, allowing us to treat it like a stream
;; using a marker as point

(defclass gtbstream (gtk-text-buffer
		     trivial-gray-streams:fundamental-character-output-stream)
  
  ((iter         :accessor iter  :initform nil :type gtk-text-iter)
   (iter1        :accessor iter1  :initform nil :type gtk-text-iter)
   (offset       :accessor offset :initform 0 :type fixnum)
   ;;local functions to buffer output

   (lbuf         :accessor lbuf   :initform (make-array 256 :element-type 'character))
   (index       :accessor index  :initform 0))
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream gtbstream) &key)
 ;; (print "init-inst GTBSTREAM")
  (with-slots (point  iter iter1) stream
    (setf iter  (gtb-get-start-iter stream)
	  iter1 (gtb-get-end-iter stream)
	  ;point (gtb-create-mark stream (null-pointer) iter nil)
	  )))
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream gtbstream))
  (gtb-delete-mark stream (point stream)))

(defmacro iter-to-offset ()
  "Set iter to point, and return it"
  `(progn (setf (gtk::gtk-text-iter-offset iter) offset) iter))

	  ;;==============================================================================
	  ;;


(defmethod trivial-gray-streams:stream-force-output ((stream gtbstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-flush stream))

(defmethod trivial-gray-streams:stream-finish-output ((stream gtbstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-flush stream))
		;;------------------------------------------------------------------------------


(defmethod trivial-gray-streams:stream-write-char ((stream gtbstream) char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-emit stream char))
		;;------------------------------------------------------------------------------


(defmethod trivial-gray-streams:stream-line-column ((stream gtbstream))
  (with-slots (iter offset) stream
    (stream-flush stream)
    (- (gti-get-line-offset (iter-to-offset)) offset)))

(defmethod trivial-gray-streams:stream-start-line-p ((stream gtbstream))
  (with-slots (iter offset) stream
    (stream-flush stream)
    (gti-starts-line (iter-to-offset))))
		;;------------------------------------------------------------------------------


(defmethod trivial-gray-streams:stream-file-position ((stream gtbstream))
  (stream-flush stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream gtbstream))
  (stream-flush stream)
  (block main
    (with-slots (iter offset) stream
      (typecase value
	(keyword (case value
		   (:start  (setf offset 0))
		   (:end    (setf offset (gtb-get-char-count stream)))
		   (t (return-from main nil))))
	(number (setf offset value))
	(t (return-from main nil)))
      t)))

(defun stream-to-iter (stream iter)
  (stream-flush stream)
  (setf (offset stream) (gti-get-offset iter))
)
		;;==============================================================================
		;; buffer routines create a closure with a buffer... These routines get hammered
		;; and really do need to be fast; a closure seems to be a little better on SBCL
		;; So here we initialize a closure, compile emit, clear and flush and set the
		;; 3 function pointers in the stream.
		;; Notes:
		;; - keep an eye on how UTF8 characters are handled!
		;; - this can be sped up with a foreign buffer as long as UTF8 is made to work.




(defun stream-emit (stream char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type character char))
  (with-slots (index lbuf) stream
    "Emit a character into stream.  May cache until..."
    
    ;;	    (format t "~%Emitting ~C into ~A at ~A" char stream index)
    (setf (schar lbuf index) char)
    (incf (the fixnum index))
    (when (or (char= char #\newline)
	      (> (the fixnum index) 250))	;buffer full, or
      (stream-flush stream)
      char)))
		;;--------------------------------------------------------------------------
		;;--------------------------------------------------------------------------



(defun stream-flush (stream)
  "Flush the stream if needed; return iter"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (iter offset index lbuf) stream
    (unless (zerop (the fixnum index))
      (iter-to-offset)
      (incf (the fixnum offset) (the fixnum index))
      (setf (schar lbuf index) #\Nul ;terminate UTF8 lbuf
	    index 0) 
      (%gtb-insert stream iter lbuf -1))
    offset))
		;;==============================================================================
		;; And our extensions...



(defun stream-wipe (stream)
  "clear the buffer entirely"
  (%gtb-delete stream
	       (gtb-get-start-iter stream)
	       (gtb-get-end-iter stream))
  (setf (index stream) 0))


(defun tagname-to-here (stream tagname start-offset)
  "apply a tag from start-offset to current position"
  (with-slots (iter iter1) stream
    (stream-flush stream) ;iter now set to end
    (%gtb-get-iter-at-offset stream iter1 start-offset);; setting iter's offset does not work
    (gtb-apply-tag stream tagname iter1 iter)))

(defun tag-to-here (stream tag start-offset)
  "apply a tag from start-offset to current position"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (iter iter1) stream
    (stream-flush stream) ;iter now set to end
    (%gtb-get-iter-at-offset stream iter1 start-offset)
    (%gtb-apply-tag stream tag iter iter1)))

		;;==============================================================================
		;;------------------------------------------------------------------------------
		;;------------------------------------------------------------------------------
		;; Tagged output
		;;
		;; As we allow nested tagged output, 'with-tags' keeps track of the tags and
		;; starting position on the stack.  The usual unwind-protect is not necessary
		;; as we do not reserve any resources
		;;


(defmacro with-tag (stream tag &body body)
  (let* ((str (gensym))
	 (off (gensym))
	 (tg (gensym)))
    `(let* ((,str (the gtbstream ,stream)) 
	    (,off (stream-flush ,stream))
	    (,tg ,tag))
       ,@body
       (tag-to-here ,str ,tg ,off))))

(defmacro with-tagname (stream tag &body body)
  (let* ((str (gensym))
	 (off (gensym))
	 (tg  (gensym)))
    `(let* ((,str (the gtbstream ,stream)) 
	    (,off (stream-flush ,str))
	    (,tg ,tag))
       ,@body
       (tagname-to-here ,str ,tg ,off))))






(defun append-presentation (stream dad pres)
  (declare (type gtbstream stream)
	   (type range:range dad))
  
  (stream-flush stream)
  (range:new-in dad pres)
  (present pres stream)
  (stream-flush stream)) 
