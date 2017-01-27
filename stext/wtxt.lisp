(in-package :stext)
;;;=============================================================================
;;; wtxt - a text widjet containin a buffer-derived class..
;;;
;;; passes button and keypress events to the rbuffer. 
;;;
(defclass wtxt (gtk-text-view)
  ()
  (:metaclass gobject-class)
  )

;; This is passed from way above to the view
(defmethod -on-destroy ((wtxt wtxt)) ;initiated by outer app window
  (-on-destroy (gtk-text-view-buffer wtxt))		;pass to active buffer
)

(defmacro make-wtxt (buffer &rest rest)
  `(make-instance 'wtxt :buffer ,buffer ,@rest))




(defgeneric -on-button-press  (object view event))
(defgeneric -on-2button-press (object view event))
(defgeneric -on-3button-press (object view event))

(defmethod -on-button-press ((o t) view event)
  (declare (ignore o iter event)))
(defmethod -on-2button-press ((o t) view event)
  (declare (ignore o iter event)))
(defmethod -on-3button-press ((o t) view event)
  (declare (ignore o iter event)))

(defun rview-buffer-coordinates (view x y)
  (gtv-window-to-buffer-coords
   view
   :text
   (truncate x)
   (truncate y)))

(defun rview-iter-from-event (view event)
  (mvb
   (x y) (gtv-window-to-buffer-coords
	  view :text
	  (truncate (gdk-event-button-x event))
	  (truncate (gdk-event-button-y event)))
   (gtv-get-iter-at-location view x y)))

(defmethod initialize-instance :after ((wtxt wtxt) &key)
  ;; Since views know about buffers but not vice versa, we must connect here.
  ;; since we don't know what the view is, we have to use generic functions.
  ;; The signaling system seems to not work well here!
  (widget-defaults wtxt); see gtk-ui.lisp
  ;;(gtk-widget-add-events wtxt (:button2-mask) )
  (g-signal-connect
   wtxt "button-press-event" ;TODO: check widget
   (lambda (view event)
     (let ((buffer (gtv-buffer wtxt)))
       (case (gdk-event-get-click-count event)
	 (1 (-on-button-press  buffer view event))
	 (2 (-on-2button-press buffer view event))
	 (3 (-on-3button-press buffer view event)))))))

(defmethod -on-announce-eli ((wtxt wtxt) eli)
  (-on-announce-eli (gtk-text-view-buffer wtxt) eli))

(defun widget-defaults (widget)
  (gtk-widget-modify-font widget  (pango-font-description-from-string "DejaVu Sans Mono 8.6"))

  (gtk-widget-override-background-color widget :normal (make-gdk-rgba :alpha 1.d0) )
  (gtk-widget-override-background-color widget :selected (make-gdk-rgba :red 0.5d0 :green 0.5d0 :blue 1.d0 :alpha 1.d0) )
  (gtk-widget-override-color widget :normal (gdk-rgba-parse "wheat")
			     ;(make-gdk-rgba :red 1.d0 :green 1.d0 :blue 1.d0 :alpha 1.d0)
			     )
  (gtk-widget-override-color widget :selected (make-gdk-rgba  :alpha 1.d0) ))

(defmethod -on-eli-key ((view wtxt) key event)
  "process an eli key"
  (-on-eli-key (gtv-buffer view) key event))
