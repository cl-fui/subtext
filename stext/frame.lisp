;;; ----------------------------------------------------------------------------
;; Frame
;;
;; contains an echobar at the bottom, and content on top.
;;
;; key processing:
;;
;; All keys are sent to minibuf (which may send them back up)
;;; ----------------------------------------------------------------------------
(in-package #:stext)

(defparameter *global-keymap* (make-keymap))


(defclass frame (gtk-window)
  ((holder   :accessor holder  :initform (make-instance 'gtk-box :orientation :vertical))
   (minibuf  :accessor minibuf :initform (make-minibuf))
   (content  :accessor content :initarg :content)
   (keysearch :accessor keysearch :initform *global-keymap*))
  (:metaclass gobject-class))

(defmethod -on-destroy ((frame frame))
  (with-slots (content) frame
    (format t "destroy:frame content ~A~&" content)
    (and content (-on-destroy content))))

(defmacro make-frame (content &rest rest)
  `(make-instance 'frame :content ,content
		 :type :toplevel
		 :default-width 640
		 :default-height 480
		 ,@rest))

(defmethod initialize-instance :after ((frame frame) &key)
  (with-slots (holder minibuf content) frame
    (gtk-box-pack-end holder minibuf :expand nil)
    (and content (gtk-box-pack-start holder content))
    (gtk-container-add frame holder)
    (g-signal-connect
     frame "destroy"
     (lambda (widget) (-on-destroy widget)
	      (leave-gtk-main)))
    (g-signal-connect frame "key-press-event" #'frame-key-press)))

(defun frame-key-press (frame event)
  (let ((gtkkey (gdk-event-key-keyval event)))
    (unless (modifier-p gtkkey); if modifier, let gtk handle it!
      (setf gtkkey (make-key gtkkey (gdk-event-key-state event)))
      (with-slots (keysearch) frame
	(let ((found (lookup-key keysearch gtkkey)))
	  (typecase found
	    (function (setf keysearch *global-keymap*)
		      (funcall found))
	    (keymap (setf keysearch found)
		    t)
	    (t nil)))))))


(defun on-key-press (eli widget event)
  "Process a key from GTK; ignore modifier keys; process other keys in eli"
  (declare (ignore widget))
  (with-slots (key interactive suspend) eli
    ))
