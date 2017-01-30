(in-package :stext)
;;------------------------------------------------------------------------------
;; gtbstream
;;
;; output at the end only!

(defclass termstream (gtk-text-buffer
		      trivial-gray-streams:fundamental-character-output-stream)
  
  ((iter         :accessor iter   :initform nil :type gtk-text-iter)
   (iter1        :accessor iter1  :initform nil :type gtk-text-iter)
   (markin       :accessor markin :initform nil :type gtk-text-mark)

   (lbuf         :accessor lbuf   :initform (make-array 4096 :element-type 'character))
   (index        :accessor index  :initform 0   :type fixnum)
   (promises     :accessor promises :initform nil))
  
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream termstream) &key)
 ;; (print "init-inst TERMSTREAM")
  (with-slots (iter iter1 markin) stream
    (setf iter   (gtb-get-start-iter stream)
	  iter1  (gtb-get-end-iter   stream)
	  markin (gtb-create-mark stream (null-pointer) iter t))))
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream termstream))
  (with-slots (markin) stream
    (%gtb-delete-mark stream markin)))



;;==============================================================================
;;
(defmethod trivial-gray-streams:stream-write-char ((stream termstream) char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-emit stream char))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-line-column ((stream termstream))
  ;;(format t "line-col~&")
  (stream-flush stream)
  (gti-get-line-offset (iter stream)))

(defmethod trivial-gray-streams:stream-start-line-p ((stream termstream))
  ;;(format t "startlinep~~&")
  (stream-flush stream)
  (gti-starts-line (iter stream)))
		;;------------------------------------------------------------------------------


(defmethod trivial-gray-streams:stream-force-output ((stream termstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(format t "Force")
  (stream-flush stream))
(defmethod trivial-gray-streams:stream-finish-output ((stream termstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(format t "Finish~~&")
  (stream-flush stream)
  )

(defmethod trivial-gray-streams:stream-file-position ((stream termstream))
  (+ (gtb-get-char-count stream)  (index stream)))

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream termstream))
  (stream-flush stream)
  nil)
;;==============================================================================
(defun stream-emit (stream char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type character char))
  (with-slots (index lbuf) stream
    "Emit a character into stream.  May cache until..."
    
    ;;	    (format t "~%Emitting ~C into ~A at ~A" char stream index)
    (setf (schar lbuf index) char)
    (incf (the fixnum index))
    (when (> (the fixnum index) 508);
      (stream-flush stream))
    char))
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
(defun stream-flush (stream)
  "Flush the stream if needed; return offset"
  (declare (optimize (speed 3) (safety 0) (debug 3)))
  
  (with-slots (iter index lbuf promises) stream
    ;;(format t "Flushing ~A characters~&" index)
    (unless (zerop (the fixnum index))
      (setf (schar lbuf (the fixnum index)) #\Nul ;terminate UTF8 lbuf
	    index 0)
      (%gtb-get-end-iter stream iter)
      (%gtb-insert stream iter lbuf -1)
      (when promises (term-promises stream)))))
;;==============================================================================
;; And our extensions...
(defun stream-wipe (stream)
  (with-slots (index iter iter1 promises) stream
    (%gtb-get-start-iter stream iter)
    (%gtb-get-end-iter   stream iter1)
    (%gtb-delete stream iter iter1)
    (setf index 0
	  promises nil)))

;;==============================================================================
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Tagged output
;;
;; As we allow nested tagged output, 'with-tags' keeps track of the tags and
;; starting position on the stack.  The usual unwind-protect is not necessary
;; as we do not reserve any resources
;;
(defstruct tag-promise tag start end)

(defun tag-in (stream tag)
  (make-tag-promise :tag tag :start (+ (gtb-get-char-count stream)
				       (index stream))))

(defun tag-out (stream promise)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type termstream stream))
  (setf (tag-promise-end promise)
	(the fixnum (+ (the fixnum (gtb-get-char-count stream))
		       (the fixnum (index stream)))))
  (push promise (promises stream)))

(defmacro with-tag (stream tag &body body)
  (let ((strm (gensym))
	(promise (gensym)))  
    `(let* ((,strm ,stream)
	   (,promise (tag-in ,strm ,tag)))
       ,@body
       (tag-out ,strm ,promise))))

(defmethod promise-fulfill ((promise tag-promise) stream)
  (with-slots (start end tag) promise
    (with-slots (iter iter1) stream
      (%gtb-get-iter-at-offset stream iter start)
      (%gtb-get-iter-at-offset stream iter1 end)
      (gtb-apply-tag stream tag iter iter1 ))))
(defun term-promises (stream)
;;  (print (promises stream))
;;  (print "---------------")
   ;;(format t "~%ROOT: ~A kids ~A~&" (root stream) (range:kids (root stream)))
  (loop for promise in (reverse (promises stream)) do
       (promise-fulfill promise stream))
  (setf (promises stream) nil))
;;------------------------------------------------------------------------------
;;


;;==============================================================================
;; Simple-Input support
;; A very simple interface to 
(defun simple-input-mark (stream)
  ;; Keep a marker as start of input.
  "Set input mark to end of buffer after flushing."
  (with-slots (iter markin) stream
    (stream-flush stream)
    (%gtb-get-end-iter stream iter)
    (%gtb-move-mark stream markin iter)
    (print
     markin)))

(defun simple-input-get-text (stream)
  "get text from markin to end"
  (with-slots (iter iter1 markin) stream
    (stream-flush stream)
    (%gtb-get-iter-at-mark stream iter markin)
    (%gtb-get-end-iter stream iter1)
    (print (gtb-get-text stream iter iter1 nil))))
