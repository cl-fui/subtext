(in-package :subtext )

(defclass modeline (gtk-text-view) ((ml :accessor ml :initform nil :initarg :ml))  (:metaclass gobject-class))

(defun make-modeline ( &key (ml nil))
  (make-instance 'modeline :buffer (make-instance 'termstream)
		 :wrap-mode :none
		 :editable nil
		 :ml ml))


;; Limit width to max-char-width... 
(defmethod initialize-instance :after ((widget modeline) &key )
  (gtk-widget-modify-font widget  (pango-font-description-from-string "DejaVu Sans Mono 8.6"))
  (gtk-widget-override-background-color widget :normal (gdk-rgba-parse "Gray85"))
  (gtk-widget-override-color widget :normal (gdk-rgba-parse "Gray10") )
  (let ((stream (gtk-text-view-buffer widget)))
    (and (ml widget) (format stream  (ml widget)))
    (finish-output stream)))
