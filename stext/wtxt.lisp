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
  (-on-destroy (gtk-text-view-buffer wtxt)) ;pass to active buffer
)

(defmacro make-wtxt (buffer &rest rest)
  `(make-instance 'wtxt :buffer ,buffer ,@rest))

(defmethod initialize-instance :after ((wtxt wtxt) &key)
  ;; Since views know about buffers but not vice versa, we must connect here.
  ;; since we don't know what the view is, we have to use generic functions.
  ;; The signaling system seems to not work well here!
  (widget-defaults wtxt); see gtk-ui.lisp

  (g-signal-connect
       wtxt "button-press-event" ;TODO: check widget
       (lambda (view event)
	 (multiple-value-bind (x y)
	     (gtk-text-view-window-to-buffer-coords
	     view :text
	      (truncate (gdk-event-button-x event))
	      (truncate (gdk-event-button-y event)))
	   (-on-button-press
	    (gtk-text-view-buffer view)
	    (gtk-text-view-get-iter-at-location view x y) event))))
  )

(defmethod -on-key-press ((wtxt wtxt) event from) ;this particular view just passes them to buffer
  (print "wtxt:on-key-press")
  (format t "~%buffer is ~A" (gtk-text-view-buffer wtxt))
  (-on-key-press (gtk-text-view-buffer wtxt) event from))


;; These are sent from the wtxt to the dynamically current buffer
(defmethod -on-key-press    ((buffer t) event from) nil)
(defmethod -on-button-press ((buffer t) iter event) nil)


(defun widget-defaults (widget)
  (gtk-widget-modify-font widget  (pango-font-description-from-string "DejaVu Sans Mono 8.6"))

  (gtk-widget-override-background-color widget :normal (make-gdk-rgba :alpha 1.d0) )
  (gtk-widget-override-background-color widget :selected (make-gdk-rgba :red 0.5d0 :green 0.5d0 :blue 1.d0 :alpha 1.d0) )
  (gtk-widget-override-color widget :normal (gdk-rgba-parse "wheat")
			     ;(make-gdk-rgba :red 1.d0 :green 1.d0 :blue 1.d0 :alpha 1.d0)
			     )
  (gtk-widget-override-color widget :selected (make-gdk-rgba  :alpha 1.d0) ))
