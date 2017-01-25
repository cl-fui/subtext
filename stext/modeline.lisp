(in-package :stext )

(defclass modeline (gtk-text-view) ()  (:metaclass gobject-class))

(defun make-modeline ()
  (make-instance 'modeline :buffer (make-instance 'rbuffer)
		 :wrap-mode :none
		 :editable nil))


;; Limit width to max-char-width... 
(defmethod initialize-instance :after ((widget modeline) &key)
  (gtk-widget-modify-font widget  (pango-font-description-from-string "DejaVu Sans Mono 8.6"))
  (gtk-widget-override-background-color widget :normal (gdk-rgba-parse "Gray85"))
  (gtk-widget-override-color widget :normal (gdk-rgba-parse "Gray10") )
  (let ((stream (gtk-text-view-buffer widget)))
    (format stream   "-:**- *slime repl*")
    (finish-output stream)))
