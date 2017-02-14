(in-package :subtext)
;;------------------------------------------------------------------------------
;; echostream - unbuffered gtk stream that erases itself on terpri.
;;

(defclass echostream (simplestream)
  ()
  (:metaclass gobject-class))
;;==============================================================================
;;
(defmethod trivial-gray-streams:stream-write-char :around ((stream echostream) char)
   (if (eq char #\newline)
      (-reset stream)
      (call-next-method stream char)))
