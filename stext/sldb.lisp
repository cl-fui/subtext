(in-package :stext)

(defclass pcondition (range:range) ())
(defclass prestart   (range:range)
    ((id :accessor id)))

(defclass pframe     (range:range)
  ((id :accessor id)
   (opn :accessor opn)))

;;-------------------------------------------------------------------
;; mouse move derived signal is called to highlight/dehighlight
;; a presentation when mouse moves over it.
(defmethod pres-highlight ((p t) stream flag))
(defmethod pres-highlight ((p pframe) stream flag)
  (mvb (start end) (range-iters stream p)
       (if flag
	   (gtb-apply-tag stream "grhigh" start end )
	   (gtb-remove-tag stream "grhigh" start end ))))

(defmethod pres-button-press ((p pframe) stream event)
  (sldb-frame-more stream p))

(defmethod pres-button-press ((p prestart) stream event)
  (sldb-invoke-restart stream (id p)))

;;OK (ql:quickload :stext)(in-package :stext)

(defun sldb-button ( view anchor text)
  (let ((widget (make-instance 'gtk-button :label text )))
    (gtk-text-view-add-child-at-anchor
     view
     widget
     anchor)
    (setf  (gtk-widget-height-request widget ) 10)
    (gtk-widget-show widget)
    ;(setf *q* widget)
    ))

;;---------------------------------------
;; SLDB presentation
(defclass -sldb (range:range)
  ((thread        :accessor thread)
   (level         :accessor thread)
   (condition     :accessor thread )
   (restarts      :accessor thread)
   (frames        :accessor thread)
   (continuations :accessor thread)))

(defclass sldb (rbuffer)
  ((connection  :accessor connection   :initarg :connection )
   (sldb-pres   :accessor sldb-pres :initarg :sldb-pres)
   (sldb-eli :accessor sldb-eli)
;;   (sldb-fr            :accessor sldb-fr :initform nil)
;;   (sldb-view          :accessor sldb-view :initform nil)
   )(:metaclass gobject-class))

(defmethod initialize-instance :after ((sldb sldb) &key)
  ;(setf *pbuf* sldb)
  (pbuf-new-tag sldb :name "grayish"  :foreground "gray" :editable nil)
  (pbuf-new-tag sldb :name "beige"  :foreground "beige" :editable nil)
  (pbuf-new-tag sldb :name "restartable"  :foreground "LimeGreen" :editable nil)
  (pbuf-new-tag sldb :name "normal"  :foreground "NavajoWhite" :editable nil)
  (pbuf-new-tag sldb :name "cyan"  :foreground "cyan" :editable nil)
  (pbuf-new-tag sldb :name "label" :foreground "Gray70" :background "Gray18" :editable nil)
  (pbuf-new-tag sldb :name "enum" :foreground "Gray70"  :editable nil)
  (pbuf-new-tag sldb :name "condition" :foreground "plum"  :editable nil)
  
  (pbuf-new-tag sldb :name "grhigh" :background "darkgreen" :foreground "NavajoWhite" ))

(defmethod -on-announce-eli :after ((sldb sldb) eli)
  (setf (sldb-eli sldb) eli)
  (with-slots (keymap) eli
    (keymap-define-key keymap #.kb:|q| (lambda (key) (sldb-quit sldb))))
 )



(defmethod -on-button-press ((sldb sldb) iter event)
  (mvb (range off) (range:at (root sldb) (gti-get-offset iter))
	(pres-button-press range sldb event)))


(let (last)
  (defmethod -on-motion ((sldb sldb) iter event)
    (let ((range (range:at (root sldb) (gti-get-offset iter))))
      (unless (eq last range)
	(pres-highlight last sldb nil)
	(pres-highlight range sldb t)
	(setf last range)))))
;;; Return a list (LOCALS TAGS) for vars and catch tags in the frame INDEX.
;;; LOCALS is a list of the form ((&key NAME ID VALUE) ...).
;;;TAGS has is a list of strings.

;; For toggling, the top line remains while the rest can disappear


(defun sldb-frame-more (sldb range)
  (with-slots (connection sldb-pres) sldb
    (with-slots (thread) sldb-pres
      (with-slots (id open) range
	(swa:emacs-rex
	 connection
	 (format nil "(swank:frame-locals-and-catch-tags ~A)" id)
	 :thread thread
	 :proc
	 (lambda (connection reply id)
	   (unless open
	     (mvb (start end) (range:bounds range)
		  (file-position sldb (1- end))
		  (format t "DDDDD ~A~% "end)
		  ;;-----------------------------
		  (format sldb "~%     Locals:")
		  (format t "EEEEE ~A~% "end)
		  (loop for item in (first (second reply)) do
		       (format sldb "~%       ~A = ~A" (second item) (sixth item)))
		  (finish-output sldb)
		  ;;-----------------------------
		  (setf open t)
		  ;;(print (first (second reply)) sldb)
		  ;;(print (second (second reply)) sldb)
		  )))))))
  )
(defparameter qqq nil)

(defun make-wsldb (connection thread level condition restarts frames continuations)
  "create the presentation, buffer and view for the debugger."
  (let* ((pres (make-instance '-sldb 
			   :thread thread
			   :level level
			   :condition condition
			   :restarts restarts
			   :frames frames
			   :continuations continuations))
	 (sldb (make-instance 
		'sldb :sldb-pres pres :connection connection)))
    (setf qqq sldb)

    (make-wtxt sldb)))

(defun wsldb-activate (wsldb)
  (sldb-activate (gtk-text-view-buffer wsldb)))

(defun wsldb-destroy (wsldb)
  (let ((frame (frame (sldb-eli (gtk-text-view-buffer wsldb)))))
    (gtk-widget-destroy frame)
    )
  
  )

(defmethod present ((p -sldb) stream)
  (format t "O1111K~&")
    (with-slots (condition restarts frames continuations) p
      (with-tagname stream "normal" 
	(format stream "~A~&" (first condition)))
      (with-range stream (make-instance 'pcondition)
	(with-tagname stream "condition"
	  (format stream "~A~&" (second condition))))
      
      (with-tagname stream "label" (format stream "~%Restarts:~&"))
      (loop for restart in restarts
	 for i from 0 do	   (with-range stream (make-instance 'prestart :id i)
	     (with-tagname stream "enum"   (format stream "~2d: [" i))
	     (with-tagname stream "cyan"   (format stream "~A" (first restart)))
	     (with-tagname stream "normal" (format stream "] ~A~&" (second restart)))))
      ;;-------------------------------------------------------
      (with-tagname stream "label" (format stream "~%Backtrace:~&"))
      (loop for frame in frames
	 for i from 0 do
	   (with-range stream (make-instance 'pframe :id i) 
	     (with-tagname stream "enum"
	       (format stream "~3d: "  (first frame)))
	     (with-tagname stream (if (third frame) "restartable" "normal")
	       (format stream "~A~&"   (second frame))))
	   )))

(defun sldb-activate (sldb)
  (format t "AAAA ~A~&" (type-of sldb))
  (present (sldb-pres sldb) sldb)
  
)


(defun sldb-invoke-restart (sldb restart)
  (with-slots (connection sldb-pres) sldb
    (with-slots (level thread) sldb-pres
      (swa:emacs-rex
       connection
       (format nil "(swank:invoke-nth-restart-for-emacs ~A ~A)" level restart)
       :thread thread))))

(defun sldb-quit (sldb)
  (with-slots (connection sldb-pres) sldb
    (with-slots (level thread) sldb-pres
      (swa:emacs-rex connection "(swank:throw-to-toplevel)" :thread thread))))



