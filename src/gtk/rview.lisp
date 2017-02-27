(in-package :subtext)
;;;=============================================================================
;;; rview - a text widget containin a buffer-derived class..
;;;
;;; 
;;;
(defgeneric -on-motion (object iter event))
(defmethod -on-motion ((o t) view event))


(defclass rview (gtk-text-view eli)
  (
   ;; eli
   (x :accessor x :initform 0)
   (y :accessor y :initform 0)
   ;; Track last offset from mousemove; issue -on-motion only on change.
   (last-motion-off :accessor last-motion-off :initform 999999) )
  (:metaclass gobject-class) )

;; a convenience macro
(defmacro make-rview (buffer &rest rest)
  `(make-instance 'rview :buffer ,buffer ,@rest))

(defun rview-key (rview key)
  "process a key..."
  (unless (eli-key-initial rview key)
    ;; (format t "RVIEW: FOUND ~A ~A~&" found partials)
    (mvb (found partials) (-on-keyseq (gtv-buffer rview) (state rview))
	 (eli-process rview found partials)))  )

(defmethod initialize-instance :after ((rview rview) &key (widget-defaults t))
  (let ((pkg *package*))
    (and widget-defaults (widget-defaults rview))		; see gtk-ui.lisp
    ;;----------------------------------------------------------------------
    (-on-announce-eli (gtv-buffer rview) rview) ; let the buffer initialize
    ;;----------------------------------------------------------------------
    ;; Key press.
    ;; Handle via eli.  Eli may just return nil which we shall pass to GTK
    ;; to use default processing in the buffer.
    (g-signal-connect
     rview "key-press-event" ;#'eli-gtk-key-press-event
     (lambda (view event)
       (let ((gtkkey (gdk-event-key-keyval event)))
	  (unless (key-is-modifier gtkkey) ; if modifier, let gtk handle it!
	    (rview-key rview (key-make
			gtkkey (gdk-event-key-state event)))))))
    ;;----------------------------------------------------------------------
    ;; Button press.
    ;; Treated as a key; convert to "Mouse-1" etc.. and report via eli
    (g-signal-connect
     rview "button-press-event" ;TODO: check widget
     (lambda (view event)
       (let ((*package* pkg) )
	 ;;(print event)
	 ;; syntesize a key event from button press
	 (mvb (w x y mod) (gdk-window-get-pointer  (gtk-widget-window view))
	      (setf (x view) x
		    (y view) y)
	      (let ((key (+ #xFEE8 (gdk-event-button-button event))))
		(rview-key rview key))))))
 #||   (g-signal-connect
     rview "drag-motion"
     (lambda (widget context x y time)
       (format t "DRAG_MOTION ~A ~A ~A ~A ~A" widget context x y time)
       nil))
||#
    ;;----------------------------------------------------------------------
    ;; Mouse motion.  We get pixel motion, but we are interested in much
    ;; coarser notification.  For now, ignore sub-character motions
    ;; and notify buffer of changes
    ;; Note: we pull a new iter every time... Potential mature optimization
    (g-signal-connect
     rview "motion-notify-event" ;TODO: check widget
     (lambda (view event)
       (let ((*package* pkg)
	     (buffer (gtv-buffer rview))
	     (iter (rview-iter-from-xy view
				       (gdk-event-motion-x event)
				       (gdk-event-motion-y event))))
	 (with-slots (last-motion-off) view 
	   (when (/= last-motion-off (gti-offset iter)); interesting?
	     (setf last-motion-off (gti-offset iter))
	     ;;(-on-motion buffer iter event)
	     (on-mouse-motion buffer iter)
	     )))))
   ))


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
