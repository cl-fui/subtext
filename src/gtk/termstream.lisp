(in-package :subtext)
;;------------------------------------------------------------------------------
;; gtbstream - streaming io, simple-input, promises
;;
;; output at the end only!

(defclass termstream (tb
		      trivial-gray-streams:fundamental-character-output-stream)
  ()
  
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream termstream) &key)
 ;; (print "init-inst TERMSTREAM")
  )
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream termstream))
  )
;;==============================================================================
;;
(defmethod trivial-gray-streams:stream-write-char ((stream termstream) char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (pbuf-emit stream char))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-line-column ((stream termstream))
  ;;(format t "line-col~&")
  (pbuf-flush stream)
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
  (pbuf-flush stream))
(defmethod trivial-gray-streams:stream-finish-output ((stream termstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(format t "Finish~~&")
  (pbuf-flush stream))

(defmethod trivial-gray-streams:stream-file-position ((stream termstream))
  (pbuf-position stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream termstream))
  (pbuf-flush stream)
  (setf (anchor stream) value); TODO: error check, and symbols
  nil)


  


(defmethod -wipe :after ((pbuf termstream))
 
  (setf (index pbuf) 0))
; should anyone use this class as a concrete class...

