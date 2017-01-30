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
   (promises     :accessor promises :initform nil)
   (active-range :accessor active-range :initform nil ))
  
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream termstream) &key)
 ;; (print "init-inst TERMSTREAM")
  (with-slots (active-range iter iter1 markin root) stream
    (setf iter   (gtb-get-start-iter stream)
	  iter1  (gtb-get-end-iter   stream)
	  markin (gtb-create-mark stream (null-pointer) iter t)
	  active-range root)))
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
  (stream-position stream))

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
(declaim (inline stream-real-position))
(defun stream-position (stream)
  (declare (optimize (speed 3) (safety 0) (debug 3)))
  (the fixnum (+ (the fixnum (gtb-get-char-count stream))
		 (the fixnum (index stream)))))

(defun stream-wipe (stream)
  (with-slots (index iter iter1 promises) stream
    (%gtb-get-start-iter stream iter)
    (%gtb-get-end-iter   stream iter1)
    (%gtb-delete stream iter iter1)
    (setf index 0
	  promises nil)))

(defun stream-apply-tag (stream tag start end)
  (with-slots (iter iter1) stream
    (%gtb-get-iter-at-offset stream iter start)
    (%gtb-get-iter-at-offset stream iter1 end)
    (gtb-apply-tag stream tag iter iter1))
)
;;==============================================================================
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Promises
;;
;; A promise is a promise to perform some task on the buffer in the future. A
;; promise has start and end offsets, and the content, such as tag..
;; We keep offsets as numeric offsets.  That should mostly work as insertion
;; happes at the end.  Should we make a promise, then insert in the beginning
;; of a buffer, bad things will happen TODO~
;;
;; Promises are made with a (promising "bold" whatever) forms.  At the end of
;; the promising form, the promise is placed on the promises list.  When the
;; output is finished,  promises are resolved.  
(defstruct promise start end content)

(defmethod promise-in (stream (content t))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type termstream stream))
  (make-promise :start (stream-position stream)
		:content content))

(defmethod promise-out (stream promise (content t))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type termstream stream)
	   (type promise promise))
  (setf (promise-end promise) (stream-position stream))
  (push promise (promises stream)))

(defmethod promise-in :after (stream (range range:range))
  (setf (range:dad range) (active-range stream)
	(active-range stream) range))

;; must be after
(defmethod promise-out :after (stream promise (range range:range))
  (setf (active-range stream) (range:dad range)
	(range:width range) (- (promise-end promise) (promise-start promise)))
  ;; At this point we have all the information to make a subrange.
  (format t "SUBRANGING ~A into ~A at offset ~A~&"
	  range
	  (range:dad range)
	  (promise-end promise))
  
  )
;; this macro requires stream to be called stream!
;; injects it for the thing promised...

(defmacro promising (thing &body body)
  (let ((promise (gensym)))
    `(let* ((it ,thing)
	    (,promise (promise-in stream it)))
       ,@body
       (promise-out stream ,promise it))))

(defmethod promise-fulfill ((tag gtk-text-tag) promise stream)
  (with-slots (start end) promise
    (stream-apply-tag stream tag start end)))

(defmethod promise-fulfill ((tag string)  promise stream )
  (with-slots (start end) promise
    (stream-apply-tag stream tag start end)))

(defmethod promise-fulfill ((range range:range) promise stream)
;;  (format t "fulfill range...~A at offset ~A - dad:~A~&"	  range () (range:dad range))
 )


(defun term-promises (stream)
 ;;; (print (promises stream))
  ;;(print "---------------")
  ;; for each promise, fullfil it
  (with-slots (promises iter iter1) stream
    (loop for promise in (promises stream) do
	 (with-slots (start end content) promise
	   (promise-fulfill content promise stream))))
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
