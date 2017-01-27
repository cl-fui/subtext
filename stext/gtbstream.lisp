(in-package :stext)
;;------------------------------------------------------------------------------
;; gtbstream
;;
;; A thin layer around gtk-text-buffer, allowing us to treat it like a stream
;; using a marker as point

(defclass gtbstream (gtk-text-buffer
		     trivial-gray-streams:fundamental-character-output-stream)
  
  ((iter         :accessor iter  :initform nil :type gtk-text-iter)
   (point        :accessor point :initform nil :type gtk-text-marker)
   (iter1        :accessor iter1  :initform nil :type gtk-text-iter)
   ;;local functions to buffer output
   (clear        :accessor clear :type function)
   (emit         :accessor emit  :type function)
   (flush        :accessor flush :type function))
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream gtbstream) &key)
 ;; (print "init-inst GTBSTREAM")
  (with-slots (point  iter iter1) stream
    (setf iter (gtb-get-end-iter stream)
	  iter1 (gtb-get-end-iter stream)
	  point (gtb-create-mark stream (null-pointer) iter nil))
    (init-buffer-routines stream)))
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream gtbstream))
  (gtb-delete-mark stream (point stream)))

(defmacro iter-at-point ()
  "Set iter to point, and return it"
  `(progn (%gtb-get-iter-at-mark stream iter point) iter))

;;==============================================================================
;;
(defmethod trivial-gray-streams:stream-force-output ((stream gtbstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (funcall (the function (flush stream))))
(defmethod trivial-gray-streams:stream-finish-output ((stream gtbstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (funcall (the function (flush stream))))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-write-char ((stream gtbstream) char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (funcall (the function (emit stream)) char))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-line-column ((stream gtbstream))
  (let ((o (stream-offset stream)))
    (- (gti-get-line-offset (iter stream)) o)))

(defmethod trivial-gray-streams:stream-start-line-p ((stream gtbstream))
  (gti-starts-line (funcall (the function (flush stream)))))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-file-position ((stream gtbstream))
  (stream-offset stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream gtbstream))
  (funcall (flush stream))
  (block main
    (with-slots (iter point) stream
      (typecase value
	(string (return-from main (when-setf point (gtb-get-mark stream value))))
	(keyword (case value
		   (:start  (%gtb-get-start-iter stream iter))
		   (:end    (%gtb-get-end-iter   stream iter))
		   (t (return-from main nil))))
	(number (%gtb-get-iter-at-offset stream iter value))
	(t (return-from main nil)))
      (gtb-move-mark stream point iter)
      t)))


;;==============================================================================
;; buffer routines create a closure with a buffer... These routines get hammered
;; and really do need to be fast; a closure seems to be a little better on SBCL
;; So here we initialize a closure, compile emit, clear and flush and set the
;; 3 function pointers in the stream.
;; Notes:
;; - keep an eye on how UTF8 characters are handled!
;; - this can be sped up with a foreign buffer as long as UTF8 is made to work.
(defun init-buffer-routines (strm)
  (let ((lbuf (make-array 256 :element-type 'character))
	(index 0)
	(stream strm))
    ;;--------------------------------------------------------------------------
    (setf (emit stream)
	  (lambda (char)
	    "Emit a character into stream.  May cache until..."
	    (declare (optimize (speed 3) (safety 0) (debug 0)))
	    (declare (type character char))
	    (declare (type fixnum index))
	    (declare (type (simple-string 256) lbuf ))
;;	    (format t "~%Emitting ~C into ~A at ~A" char stream index)
	    (setf (schar lbuf index) char)
	    (incf index)
	    (when (or (char= char #\newline)
		      (> index 250)) ;buffer full, or
	      (funcall (the function (flush stream)))
	      char)))
    ;;--------------------------------------------------------------------------
    (setf (clear stream)
	  (lambda ()
	    "clear the buffer"
	    (setf index 0)))
    ;;--------------------------------------------------------------------------
    (setf (flush stream)
	  (lambda ()
	    "Flush the stream if needed; return iter"
	    ;; Uses iter
	    (declare (type fixnum index))
	    (declare (type (simple-string 256) lbuf ))
;;	    (format t "~%Flush ~A characters" index )
	    (with-slots (iter point) stream
	      (prog1
		  (iter-at-point)
		(unless (zerop index)
		  (setf (schar lbuf index) #\Nul ;terminate UTF8 lbuf
			index 0) 
		  (%gtb-insert stream iter lbuf -1))))))))
;;==============================================================================
;; And our extensions...

(defun stream-wipe (stream)
  "clear the buffer entirely"
  (%gtb-delete stream
	       (gtb-get-start-iter stream)
	       (gtb-get-end-iter stream)))

(defun stream-offset (stream)
  "get a clean offset"
  (declare (optimize (speed 3) (safety 0)))
  (gti-offset (funcall (the function (flush stream)))))

(defun tag-to-here (stream tag start-offset)
  "apply a tag from start-offset to current position"
  (funcall (the function (flush stream)))
  (%gtb-get-iter-at-offset stream (iter1 stream) start-offset) ; old
  (gtb-apply-tag
	  stream tag
	  (iter1 stream)
	  (iter stream)))
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
  (let ((str (gensym))
	(off (gensym))
	(tagg (gensym)))
    `(let* ((,str (the gtbstream ,stream)) 
	    (,off (stream-offset ,str))
	    (,tagg ,tag))
       ,@body
       (tag-to-here ,str ,tagg ,off))))






(defun append-presentation (stream dad pres)
  (declare (type gtbstream stream)
	   (type range:range dad))
  (with-slots (flush) stream
    (declare (type function flush))
    (funcall flush)
    (range:new-in dad pres)
    (present pres stream)
    (funcall flush)))
