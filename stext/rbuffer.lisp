(in-package :stext)
;;;=============================================================================
;;; rbuffer - a gtk-text-buffer 
;;;
;;OK (ql:quickload :stext)(in-package :stext)
(defclass rbuffer (termstream)
  ()
(:metaclass gobject-class))
