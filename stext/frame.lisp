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

(defgeneric -on-eli-key (object key event)) ; return nil to keep processing


(defparameter *frame* nil)
(defclass frame (gtk-window)
  ((holder   :accessor holder  :initform (make-instance 'gtk-box :orientation :vertical))
   (minibuf  :accessor minibuf )
   (content  :accessor content :initarg :content))
  (:metaclass gobject-class))

(defmethod -on-destroy ((frame frame))
  (with-slots (content) frame
    (format t "destroy:frame content ~A~&" content)
    (and content (-on-destroy content))
    ))

(defmacro make-frame (content &rest rest)
  `(make-instance 'frame :content ,content
		 :type :toplevel
		 :default-width 640
		 :default-height 480
		 ,@rest))


(defmethod -on-initial-display ((frame frame))
  (with-slots (minibuf content) frame
    (-on-initial-display minibuf)
    (-on-initial-display content)))

(defmethod initialize-instance :after ((frame frame) &key kill)
  (with-slots (holder minibuf content) frame
    (setf *frame* frame)

    (setf minibuf (make-minibuf frame))
    (gtk-box-pack-end holder minibuf :expand nil)
    (and content (gtk-box-pack-start holder content))
    (gtk-container-add frame holder)

    (g-signal-connect
     frame "destroy"
     (lambda (widget) (-on-destroy widget)
	     (if kill (leave-gtk-main))))

    ;; process keystrokes in minibuf...
   (-on-announce-eli content minibuf)
    
    
    ;; Key processing: gtk stuff is done here, from here on we use
    ;; (-on-eli-key object key)
    ;;
    (g-signal-connect
	frame "key-press-event"
	(lambda (frame event)
	;;  (format t "FRAME:KEY ~A~&" event)
	  
	  (let ((gtkkey (gdk-event-key-keyval event)))
	    (unless (eli:key-is-modifier gtkkey)	; if modifier, let gtk handle it!
	      (let ((key (eli:key-make gtkkey (gdk-event-key-state event))))
		(-on-eli-key (minibuf frame) key event)
	#||	(or (-on-eli-key (minibuf frame) key event)
		    (-on-eli-key (content frame) key event)
					;(format t "~&UNHANDLED KEY: ~A ~A~&" key event)
		    )
		||#
		)))

))))




