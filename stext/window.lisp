;;; ----------------------------------------------------------------------------
;; Window
;;
;; A window is a gtk widget, containing a modeline and a text widget...
;;
;; 
;;; ----------------------------------------------------------------------------
(in-package #:stext)
(defclass window (gtk-box)
  ((view     :accessor view     :initform nil   :initarg :view)
   (modeline :accessor modeline :initform (make-modeline)))
  (:metaclass gobject-class))

(defun make-window (content)
  (make-instance 'window
		 :view content
		 :orientation :vertical))


(defmethod initialize-instance :after ((window window) &key)
  (with-slots (view modeline) window
    (let ((scrolled (make-instance 'gtk-scrolled-window
				   :border-width 0
				   :hscrollbar-policy :automatic
				   :vscrollbar-policy :automatic)))
      (gtk-container-add scrolled view)
      (gtk-box-pack-start window scrolled)
      (gtk-box-pack-end window modeline :expand nil))))

(defmethod -on-announce-eli ((window window) eli)
  (-on-announce-eli (view window)  eli))

(defmethod -on-destroy ((window window))
  ;;(print "destroy:window")
  (-on-destroy (view window))
  (-on-destroy (modeline window)))

(defmethod -on-eli-key ((window window) key event)
   (-on-eli-key (view window) key event))

