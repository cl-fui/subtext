(in-package :subtext)
;;------------------------------------------------------------------------------
;; gtbstream - streaming io, simple-input, promises
;;
;; output at the end only!

(defclass termstream (basebuf
		      trivial-gray-streams:fundamental-character-output-stream)
  
  ((markin       :accessor markin :initform nil :type gtk-text-mark)
   (lbuf         :accessor lbuf   :initform (make-array 4096 :element-type 'character))
   (index        :accessor index  :initform 0   :type fixnum)
  
   ;; for now keep presentations in an array indexed by string in tag
  )
  
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream termstream) &key)
 ;; (print "init-inst TERMSTREAM")
  (with-slots (active-range iter iter1 markin root promise-free-list) stream
    (setf markin (gtb-create-mark stream (null-pointer) iter t))    ))
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
  (with-slots (index lbuf iter anchor) stream
  #||  (format t "startlinep ~A ~C~& "
	    (subseq lbuf (max 0 (- index 5)) index)
	    (if (> 0 index) (schar lbuf (1- index)) #\_))
    ||#
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
    (when (> (the fixnum index) 4090);
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



;;==============================================================================
;; Simple-Input support
;; A very simple interface to 
(defun simple-input-mark (stream)
  ;; Keep a marker as start of input.
  "Set input mark to end of buffer after flushing."
  (with-slots (iter markin) stream
    (stream-flush stream)
    (%gtb-get-end-iter stream iter) ;in this case, really want an end iter!
    (%gtb-move-mark stream markin iter)))

(defun simple-input-get-text (stream)
  "get text from markin to end"
  (with-slots (iter iter1 markin) stream 
    (simple-input-iters stream) 
    (gtb-get-text stream iter iter1 nil)))

(defun simple-input-iters (stream)
  "set iters to start and end"
  (with-slots (iter iter1 markin) stream
    (stream-flush stream)
    (%gtb-get-iter-at-mark stream iter markin)
    (%gtb-get-end-iter stream iter1))
)
;;----------------------------------------------------------------------
;; A promise
(defun simple-input-promise (stream content)
  "make a promise on a range of last input"
  (with-slots (iter iter1 promises) stream
    (simple-input-iters stream)
    (push (make-promise  :start (gti-offset iter)
			 :end (gti-offset iter1)
			 :content content)
	  promises)))


;;------------------------------------------------------------------------------

;;(defmethod -on-button-press ((sldb sldb) iter event))

  ;; get the presentation
;;  (mvb (range off) (range:actual (root sldb) (gti-get-offset iter))	(pres-button-press range sldb event))
  


(defmethod -wipe :after ((pbuf termstream))
 
  (setf (index pbuf) 0))
; should anyone use this class as a concrete class...
(defmethod -on-announce-eli ((pbuf termstream) eli)  )
