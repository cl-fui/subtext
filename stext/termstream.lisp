(in-package :stext)
;;------------------------------------------------------------------------------
;; gtbstream
;;
;; output at the end only!

(defclass termstream (basebuf
		      trivial-gray-streams:fundamental-character-output-stream)
  
  ((markin       :accessor markin :initform nil :type gtk-text-mark)

   (lbuf         :accessor lbuf   :initform (make-array 4096 :element-type 'character))
   (index        :accessor index  :initform 0   :type fixnum)
   (promises     :accessor promises :initform nil)
   (active-range :accessor active-range :initform nil )

   (promise-free-list :accessor promise-free-list :initform nil))
  
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream termstream) &key)
 ;; (print "init-inst TERMSTREAM")
  (with-slots (active-range iter iter1 markin root promise-free-list) stream
    (setf markin (gtb-create-mark stream (null-pointer) iter t)
	  active-range (range:make))
	  
    (loop for i from 0 to 200 do
	 (push (make-promise :start 0 :end 0 :content nil) promise-free-list))))
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
  (format t "line-col~&")
  (stream-flush stream)
  (gti-get-line-offset (iter stream)))

(defmethod trivial-gray-streams:stream-start-line-p ((stream termstream))
  (with-slots (index lbuf iter anchor) stream
    (format t "startlinep ~A~& " (subseq lbuf (max 0 (- index 5)) index))
    (if (zerop index) ;if buffer has just been flushed, no way to check
	(progn ;except ask gtk...
	  (%gtb-get-iter-at-offset stream iter anchor)
	  (%gti-starts-line iter))
	(char= #\newline (schar lbuf (1- index))))))
;;------------------------------------------------------------------------------


(defmethod trivial-gray-streams:stream-force-output ((stream termstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(format t "Force")
  (stream-flush stream))
(defmethod trivial-gray-streams:stream-finish-output ((stream termstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(format t "Finish~~&")
  (stream-flush stream))

(defmethod trivial-gray-streams:stream-file-position ((stream termstream))
  (stream-position stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream termstream))
  (stream-flush stream)
  (setf (anchor stream) value); TODO: error check, and symbols
  nil)
;;==============================================================================
(defun stream-emit (stream char)
  "Emit a character into stream.  May cache until..."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type character char))
  (with-slots (index lbuf active-range) stream
;;    (format t "~%Emitting ~C into ~A at ~A" char stream index)
;;    (print (range:childest active-range))
    (setf (schar lbuf index) char)
    (incf (the fixnum index))
    ;; In order to build detached range structures, widen the 'active range'
    (range::widen-prim (range:childest active-range) 1 )
    (when (> (the fixnum index) 508);
      (stream-flush stream))
    char))
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
(defun stream-flush (stream)
  "Flush the stream if needed; return offset"
  ;(declare (optimize (speed 3) (safety 0) (debug 3)))
  (with-slots (iter index lbuf promises anchor ) stream
;;    (format t "Flushing ~A characters to ~A~&" index anchor)
;;    (format t "PROMISES ~A~&" promises)
;;    (print "--------------------")
;;    (print (root stream))
    (unless (zerop (the fixnum index))
      (setf (schar lbuf (the fixnum index)) #\Nul ;terminate UTF8 lbuf
	    index 0)
      (%gtb-get-iter-at-offset stream iter anchor)
      (%gtb-insert stream iter lbuf -1)
      (when promises (promises-fulfill stream)))))
;;==============================================================================
;; And our extensions...


(defun stream-position (stream)
     (declare (optimize (speed 3) (safety 0) (debug 3)))
     (the fixnum (+ (the fixnum (anchor stream))
		    (the fixnum (index stream)))))

(defun stream-wipe (stream)
  (with-slots (index iter iter1 promises anchor) stream
    (pbuf-bounds stream)
    (%gtb-delete stream iter iter1)
    (setf index 0
	  anchor 0
	  promises nil)))

