(in-package :subtext)
;;;=============================================================================
;;; rview - a text widget containin a buffer-derived class..
;;;
;;; passes button and keypress events to the rbuffer. 
;;;
(defgeneric -on-motion (object iter event))
(defmethod -on-motion ((o t) view event))


(defclass rview (gtk-text-view)
  (
   ;; eli
   (state :accessor state
	  :documentation "first= binding during search, rest are previous bindings")
   (keymap :accessor keymap :initarg :keymap :initform nil)
   (x :accessor x :initform 0)
   (y :accessor y :initform 0)
   ;; Track last offset from mousemove; issue -on-motion only on change.
   (last-motion-off :accessor last-motion-off :initform 999999) )
  (:metaclass gobject-class) )

;; a convenience macro
(defmacro make-rview (buffer &rest rest)
  `(make-instance 'rview :buffer ,buffer ,@rest))

(defmethod initialize-instance :after ((rview rview) &key)
  ;; Since views know about buffers but not vice versa, we must connect here.
  ;; since we don't know what the view is, we have to use generic functions.
  ;; The signaling system seems to not work well here!
  (widget-defaults rview); see gtk-ui.lisp
  ;;(gtk-widget-add-events rview (:button2-mask) )
  ;;----------------------------------------------------------------------
  ;; Mouse button.  Multiple clicks are stupid, as they all get called...
  ;;
  (eli-initialize)

  (with-slots (state keymap ) rview
    (unless keymap (setf keymap (keymap-make)))
    (eli-reset rview)
    ;; built-in bindings
    (eli-def rview (kbd "C-g") (lambda () (eli-reset rview)))
    )

  (-on-announce-eli (gtv-buffer rview) rview) ; let the buffer initialize

  ;;----------------------------------------------------------------------
  ;; Key press.
  ;; Handle via eli.  Eli may just return nil which we shall pass to GTK
  ;; to use default processing in the buffer.
  (g-signal-connect
	rview "key-press-event"
	(lambda (widget event)
	 ;; (format t "FRAME:KEY ~A~&" event)
	  (let ((gtkkey (gdk-event-key-keyval event)))
	    (unless (key-is-modifier gtkkey)	; if modifier, let gtk handle it!
	      (let ((key (key-make gtkkey (gdk-event-key-state event))))
		(process-key rview key event) )))))
  ;;----------------------------------------------------------------------
  ;; Mouse motion.  We get pixel motion, but we are interested in much
  ;; coarser notification.  For now, ignore sub-character motions
  ;; and notify buffer of changes
  (g-signal-connect
   rview "motion-notify-event" ;TODO: check widget
   (lambda (view event)
     (let ((buffer (gtv-buffer rview))
	   (iter (rview-iter-from-xy view
				     (gdk-event-motion-x event)
				     (gdk-event-motion-y event))))
       (with-slots (last-motion-off) view 
	 (when (/= last-motion-off (gti-offset iter)); interesting?
	   (setf last-motion-off (gti-offset iter))
	   (-on-motion buffer iter event))))))
  ;;----------------------------------------------------------------------
  ;; Button press.
  ;; Treated as a key; convert to "Mouse-1" etc.. and report via eli
  (g-signal-connect
     rview "button-press-event" ;TODO: check widget
     (lambda (view event)
       ;;(print event)
       ;; syntesize a key event from button press
       ;(+ #xFEE9 (gdk-event-button-button event))
       (mvb (w x y mod) (gdk-window-get-pointer  (gtk-widget-window view))
	    (setf (x view) x
		  (y view) y)
	    (let ((key (+ #xFEE8 (gdk-event-button-button event))))
	      (process-key view (gtkmods-subject key mod nil) event)))
       t)))


;; pass some messages to buffer
(defmethod -on-initial-display ((rview rview))
  (-on-initial-display (gtv-buffer rview)))

(defmethod -pre-initial-display ((rview rview) frame)
  "after everything initialized but prior to display"
  )
(defmethod -on-destroy ((rview rview))
  (-on-destroy (gtv-buffer rview)))
;;==============================================================================
;;

;;==============================================================================
;; Some convenience functions
;;
(defun rview-buffer-coordinates (view x y)
  (gtv-window-to-buffer-coords
   view
   :text
   (truncate x)
   (truncate y)))
;;----------------------------------------------------------------------
;; Extract an iter from window coordinates, from mouse events
(defun rview-iter-from-xy (view x y)
  (mvb (xx yy)
       (gtv-window-to-buffer-coords
	view :text (truncate x) (truncate y))
       (gtv-get-iter-at-location view xx yy)))



(defun widget-defaults (widget)
  (gtk-widget-modify-font widget  (pango-font-description-from-string "DejaVu Sans Mono 8.6"))

  (gtk-widget-override-background-color widget :normal (make-gdk-rgba :alpha 1.d0) )
  (gtk-widget-override-background-color widget :selected (make-gdk-rgba :red 0.5d0 :green 0.5d0 :blue 1.d0 :alpha 1.d0) )
  (gtk-widget-override-color widget :normal (gdk-rgba-parse "wheat")
			     ;(make-gdk-rgba :red 1.d0 :green 1.d0 :blue 1.d0 :alpha 1.d0)
			     )
  (gtk-widget-override-color widget :selected (make-gdk-rgba  :alpha 1.d0) ))



;;============================================================================
;; used by swarepl to build sldb
(defmacro make-framed-view (view &rest frame-stuff)
  `(let ((frame (make-frame (make-window ,view)
			    ,@frame-stuff)))
     (gtk-widget-show-all frame)
     frame))

(defun rview-destroy-top (widget)
  (and widget
       (if (gtk-widget-is-toplevel widget)
	   (gtk-widget-destroy widget)
	   (rview-destroy-top (gtk-widget-parent widget)))))
