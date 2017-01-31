(in-package :stext)
;;;=============================================================================
;;; rview - a text widget containin a buffer-derived class..
;;;
;;; passes button and keypress events to the rbuffer. 
;;;

(defclass rview (gtk-text-view)
  ()
  (:metaclass gobject-class) )

;; a convenience macro
(defmacro make-rview (buffer &rest rest)
  `(make-instance 'rview :buffer ,buffer ,@rest))

;; pass some messages to buffer
(defmethod -on-initial-display ((rview rview))
  (-on-initial-display (gtv-buffer rview)))

(defmethod -on-destroy ((rview rview)) 
  (-on-destroy (gtv-buffer rview)))


(defgeneric -on-button-press  (object iter event))
(defgeneric -on-2button-press (object iter event))
(defgeneric -on-3button-press (object iter event))
(defgeneric -on-motion        (object iter event))
(defmethod -on-motion ((o t) view event)
  (declare (ignore o view event)))

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

(defun rview-iter-from-xy (view x y)
  (mvb
   (xx yy) (gtv-window-to-buffer-coords
	  view :text x y)
   (gtv-get-iter-at-location view xx yy)))


(defmethod initialize-instance :after ((rview rview) &key)
  ;; Since views know about buffers but not vice versa, we must connect here.
  ;; since we don't know what the view is, we have to use generic functions.
  ;; The signaling system seems to not work well here!
  (widget-defaults rview); see gtk-ui.lisp
  ;;(gtk-widget-add-events rview (:button2-mask) )
  (g-signal-connect
   rview "button-press-event" ;TODO: check widget
   (lambda (view event)
     (let ((buffer (gtv-buffer rview))
	   (iter (rview-iter-from-xy view
				     (truncate (gdk-event-button-x event))
				     (truncate (gdk-event-button-y event)))))
       (case (gdk-event-get-click-count event)
	 (1 (-on-button-press  buffer iter event))
	 (2 (-on-2button-press buffer iter event))
	 (3 (-on-3button-press buffer iter event))))))
  (g-signal-connect
   rview "motion-notify-event" ;TODO: check widget
   (lambda (view event)
     (let ((buffer (gtv-buffer rview))
	   (iter (rview-iter-from-xy view
					(truncate (gdk-event-motion-x event))
					(truncate (gdk-event-motion-y event)))))
       (-on-motion buffer iter event)))))
;; Looks like view is the place to handle cursor commands! TODO: improve...
(defmethod -on-announce-eli ((rview rview) eli)
#||  (defun bind-move-cursor (gtkkey)
    (apply #'g-signal-emit rview  "move-cursor"
	   (case gtkkey
	     (#.kb:LEFT  '(:visual-positions -1))
	     (#.kb:RIGHT '(:visual-positions  1))
	     (#.kb:UP    '(:display-lines    -1))
	     (#.kb:DOWN  '(:display-lines     1))
	     (t (print "FUCK YOU~&")nil))))
  (with-slots (keymap) eli
    (mapc (lambda (key) (keymap-define-key keymap key  #'bind-move-cursor) )
	  '(#.kb:LEFT #.kb:RIGHT #.KB:UP #.kb:DOWN))
    )
||#
  (-on-announce-eli (gtk-text-view-buffer rview) eli))

(defun widget-defaults (widget)
  (gtk-widget-modify-font widget  (pango-font-description-from-string "DejaVu Sans Mono 8.6"))

  (gtk-widget-override-background-color widget :normal (make-gdk-rgba :alpha 1.d0) )
  (gtk-widget-override-background-color widget :selected (make-gdk-rgba :red 0.5d0 :green 0.5d0 :blue 1.d0 :alpha 1.d0) )
  (gtk-widget-override-color widget :normal (gdk-rgba-parse "wheat")
			     ;(make-gdk-rgba :red 1.d0 :green 1.d0 :blue 1.d0 :alpha 1.d0)
			     )
  (gtk-widget-override-color widget :selected (make-gdk-rgba  :alpha 1.d0) ))

(defmethod -on-eli-key ((view rview) key event)
  "process an eli key"
  (-on-eli-key (gtv-buffer view) key event))
