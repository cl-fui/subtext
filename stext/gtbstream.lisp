(in-package :stext)
;;------------------------------------------------------------------------------
;; gtbstream
;;
;; A thin layer around gtk-text-buffer, allowing us to treat it like a stream
;; using cursor as point

(defclass gtbstream (gtk-text-buffer
		     trivial-gray-streams:fundamental-character-output-stream)
  
  ((iter         :type gtk-text-iter
		 :initform nil
		 :accessor iter)
   (point        :initform nil
		 :accessor point)
   ;;local functions to buffer output
   (clear        :accessor clear :type function)
   (emit         :accessor emit :type function)
   (flush        :accessor flush :type function))
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream gtbstream) &key)
  (print "init-inst GTBSTREAM")
  (with-slots (point  iter) stream
    (setf iter (gtb-get-end-iter stream)
	  point (gtb-get-mark stream "insert"))
     (buffer-routines stream))
  )
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream gtbstream))
  (print "GTBSTREAM ON-DESTROY")
)



(defmacro iter-at-point ()
  "Set iter to point, and return it"
  `(progn (%gtb-get-iter-at-mark stream iter point) iter))
;;==============================================================================
(defun buffer-routines (it)

  (let ((lbuf (make-array 256 :element-type 'character))
	(index 0)
	(stream it))
    (print "BUFFER ROUTINES")
    (print it)
    ;;--------------------------------------------------------------------------
    (setf (emit stream)
	  (lambda (char)
	    "Emit a character into stream.  May cache until..."
	    (declare (optimize (speed 3) (safety 0) (debug 0)))
	    (declare (type character char))
	    (declare (type fixnum index))
	    (declare (type (simple-string 256) lbuf ))
	    ;;(format t "~%Emitting ~C into ~A at ~A" char stream index)
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
	    ;;(format t "~%Flush ~A characters from ~A~&" index stream )
	    (with-slots ( iter point ) stream
	      (unless (zerop index)
		(setf (schar lbuf index) (code-char 0);null-terminate lbuf
		      index 0)
		(gtb-insert-at-cursor stream lbuf -1);watch out for UTF8 chars!
		(iter-at-point))
	      iter)))))


;;==============================================================================
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-force-output ((stream gtbstream))
  (funcall (flush stream)))
(defmethod trivial-gray-streams:stream-finish-output ((stream gtbstream))
  (funcall (flush stream)))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-write-char ((stream gtbstream) char)
  (funcall (emit stream) char))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-line-column ((stream gtbstream))
  (let ((o (gti-get-line-offset (funcall (flush stream)))))
    ;;(print o)
    o))
(defmethod trivial-gray-streams:stream-start-line-p ((stream gtbstream))
  (let ((o (gti-starts-line (funcall (flush stream)))))
    ;;(print o)
    o)
)
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-file-position ((stream gtbstream))
  (funcall (flush stream))
  (with-slots (iter point) stream
    (gti-get-offset (iter-at-point))))

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream gtbstream))
  (funcall (flush stream))
  (with-slots (iter point)stream
    (case value
      (:start (%gtb-get-iter-at-offset stream iter 0))
      (:end   (%gtb-get-end-iter stream iter))
      (t
       (%gtb-get-iter-at-offset stream iter value)))
    (gtb-place-cursor stream iter)
    t))


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Tagged output
;;
;; As we allow nested tagged output, 'with-tags' keeps track of the tags and
;; starting position on the stack.
;;
(defmacro with-tag (stream tag &body body)
  (let ((str (gensym)))
    `(let ((,str ,stream ))
       (with-slots (iter point flush) ,str
	 (let ((off-start (gti-get-offset (funcall flush))))
	   (unwind-protect
		(progn ,@body)
	     (let ((end (funcall flush)))
	       (gtb-apply-tag
		,str ,tag
		(gtb-get-iter-at-offset ,str off-start) ; old
		end ))))))) )





