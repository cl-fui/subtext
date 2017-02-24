(in-package :subtext)
;;------------------------------------------------------------------------------
;; Markstream.
;;
;; A alongside of buffer, provides streaming functionality.
;;
(defclass mark-out-stream
    (tb trivial-gray-streams:fundamental-character-output-stream)
  ((mark         :accessor mark   :initform nil :type gtk-text-mark)
   (promises     :accessor promises :initform nil)
   (lbuf         :accessor lbuf   :initform (make-array 4096 :element-type 'character))
   (index        :accessor index  :initform 0   :type fixnum))
  (:metaclass gobject-class)
  )

;;==============================================================================
(defmethod initialize-instance :after ((stream mark-out-stream) &key)
  (with-slots (iter mark) stream
    (setf iter (gtb-get-end-iter stream)
	  mark (gtb-create-mark stream (null-pointer) iter nil))))
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream mark-out-stream))
  (with-slots (mark) stream
    (%gtb-delete-mark stream mark)))

(defun stream-iter-to-mark (stream)
  "Set iter to mark"
  (with-slots (iter mark) stream
    (%gtb-get-iter-at-mark stream iter mark)
    iter))


;;==============================================================================
;;
(defmethod trivial-gray-streams:stream-write-char ((stream mark-out-stream) char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-emit stream char))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-line-column ((stream mark-out-stream))
  (stream-flush stream)
  (gti-get-line-offset (stream-iter-to-mark stream)))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-start-line-p ((stream mark-out-stream))
  (with-slots (index iter lbuf mark) stream
    (if (zerop index) ;if buffer has just been flushed, GTK knows
	(%gti-starts-line (stream-iter-to-mark stream))
	(char= #\newline (schar lbuf (1- index))))))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-force-output ((stream mark-out-stream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(format t "Force")
  (stream-flush stream))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-finish-output ((stream mark-out-stream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(format t "Finish~~&")
  (stream-flush stream))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-file-position ((stream mark-out-stream))
  (stream-position stream))

;;------------------------------------------------------------------------------
(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream mark-out-stream))
  (with-slots (iter mark) stream
    (stream-flush stream)
    (%gtb-get-iter-at-offset stream iter value)
    (%gtb-move-mark stream mark iter))
  nil)


;;==============================================================================
;; Stream support - cached write
;;
(defun stream-emit (stream char)
  "Emit a character into the cache"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type character char))
  (with-slots (index lbuf mark) stream
;;    (format t "~%Emitting ~C into ~A at ~A" char stream index)
;;    (print (range:childest active-range))
    (setf (schar lbuf index) char)
    (incf (the fixnum index))
    ;; In order to build detached range structures, widen the 'active range'
    (when (> (the fixnum index) 4090);
      (stream-flush stream))
    char))
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
(defun stream-flush (stream)
  "Flush the cache if needed"
  ;(declare (optimize (speed 3) (safety 0) (debug 3)))
  (with-slots (iter index lbuf mark promises) stream
    (unless (zerop (the fixnum index))
      (setf (schar lbuf (the fixnum index)) #\Nul ;terminate UTF8 lbuf
	    index 0)
      (%gtb-get-iter-at-mark stream iter mark)
      (%gtb-insert stream iter lbuf -1)
      (when promises (promises-fulfill stream)))))

(defun stream-position (stream)
  (declare (optimize (speed 3) (safety 0) (debug 3)))
  (with-slots (index iter mark) stream
    (%gtb-get-iter-at-mark stream iter mark)
    (the fixnum (+ (the fixnum (gti-offset iter))
		   (the fixnum index)))))


(defmethod -reset ((stream mark-out-stream))
  (-wipe stream)  )
