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

(defparameter *global-keymap* (keymap-make))


(defclass frame (gtk-window)
  ((holder   :accessor holder  :initform (make-instance 'gtk-box :orientation :vertical))
   (minibuf  :accessor minibuf )
   (content  :accessor content :initarg :content))
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
    (setf minibuf (make-minibuf frame))
    (gtk-box-pack-end holder minibuf :expand nil)
    (and content (gtk-box-pack-start holder content))
    (gtk-container-add frame holder)
    (g-signal-connect
     frame "destroy"
     (lambda (widget) (-on-destroy widget)
	     (leave-gtk-main)))
    ;; process keystrokes in minibuf...
    (-on-announce-eli content minibuf)
    (g-signal-connect frame "key-press-event"
		      (lambda (frame event)
			(-on-key-press (minibuf frame) event nil)))))




