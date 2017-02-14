(in-package :subtext)
;;------------------------------------------------------------------------------
;; simplestream - an unbuffered output stream
;;

(defclass simplestream (basebuf
			trivial-gray-streams:fundamental-character-output-stream)
  ()
  (:metaclass gobject-class))
;;==============================================================================
;;
(defmethod trivial-gray-streams:stream-write-char ((stream simplestream) char)
  (let ((s (format nil "~C" char)))
    (with-slots (iter anchor ) stream
      (%gtb-get-iter-at-offset stream iter anchor)
      (%gtb-insert stream iter s -1))))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-line-column ((stream simplestream))
  ;;(format t "line-col~&")
  (with-slots (anchor iter) stream
    (%gtb-get-iter-at-offset stream iter anchor)
    (gti-get-line-offset (iter stream))))

(defmethod trivial-gray-streams:stream-start-line-p ((stream simplestream))
  (with-slots (anchor iter) stream
    (%gtb-get-iter-at-offset stream iter anchor)
    (%gti-starts-line iter)))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-force-output ((stream simplestream))
)
(defmethod trivial-gray-streams:stream-finish-output ((stream simplestream))
)

(defmethod trivial-gray-streams:stream-file-position ((stream simplestream))
  (anchor stream)
)

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream simplestream))
  (stream-flush stream)
  (setf (anchor stream) value); TODO: error check, and symbols
  nil)


(defmethod -reset ((pbuf simplestream))
  (-wipe pbuf)  )
