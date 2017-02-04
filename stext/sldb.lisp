(in-package :stext)
#||
(defclass psldb-line (range:range) ())

(defclass pcondition (range:range) ())

;====================================================================
(defclass pframe     (psldb-line)
  ((id   :accessor id    :initarg :id   )
   (desc :accessor desc  :initarg :desc )
   (restartable :accessor restartable :initarg :restartable)
   
   (pframex :accessor pframex :initform nil)
   (expand :accessor expand :initform nil)))

(defmethod pres-button-press ((p pframe) stream event)
  (format t "press~&")
  (with-slots (expand) p
    (setf expand (not expand))
    (pframe-toggle p stream expand)
    ))

(defmethod present ((p pframe) stream extra)
  (with-slots (id desc restartable pframex) p
    (with-tag  "enum" (format stream "~3d: " id))
    (with-tag  (if restartable "restartable" "normal")
      (format stream "~A" desc))))

(defun pframe-toggle (p stream expand)
  (with-slots (pframex) p
    (with-slots (iter iter1) stream
      (pbuf-range-iters stream pframex)
      (format t "FRA<EX ~A~&" pframex)
      (setf *r* pframex)
      (if expand
	  (progn
	    (with-slots (connection thread) stream
	      (with-slots (id opn) p
		(swa:emacs-rex
		 connection
		 (format nil "(swank:frame-locals-and-catch-tags ~A)" id)
		 :thread thread
		 :proc
		 (lambda (connection reply id)
		   (file-position stream (gti-offset iter))
		   (print (active-range stream))
		   (terpri stream) (princ  "     " stream)
		   (with-tag "grayloc:" (princ "Locals:" stream))
		   (loop for item in (first (second reply)) do
			(terpri stream)
			(princ "      " stream)
			(with-tag "locleft" (princ (second item) stream))
			(with-tag "normal"  (princ " = "  stream))
			(with-tag "pres"    (princ (sixth item) stream)))
		   (finish-output stream))))))
	  ;; (gtb-insert stream	      (format nil "~&FUCK YOU, dickwad") :position iter)
	  (progn
	    (format t "TYRING TO ELIMINATE ~A~&" pframex) 
	    (pbuf-range-minimize stream pframex))))
    )
  
  )

;;;====================================================================
;;; Communicates live with swank!
;;
(defclass pframex    (range:range)
  ((connection :accessor connection :initarg :connection)
   (opn :accessor opn  :initarg :opn :initform nil )))

(defmethod present ((p pframex) stream extra)
  (with-slots (id desc restartable) p
    (with-tag  "enum" (format stream "~3d: " id))
    (with-tag  (if restartable "restartable" "normal")
      (format stream "~A" desc))
    ))
;;;====================================================================
(defclass prestart   (psldb-line)
  ((id :accessor id :initarg :id   :initform nil)))

(defmethod pres-button-press ((p prestart) stream event)
  (print "INVOKING RESTART")
  (sldb-invoke-restart stream (id p)))

;;-------------------------------------------------------------------
;; mouse move derived signal is called to highlight/dehighlight
;; a presentation when mouse moves over it.
(defmethod pres-highlight ((p t) stream flag))
(defmethod pres-highlight ((p psldb-line) stream flag)
  (pbuf-range-iters stream p)
  (with-slots (iter iter1) stream
    (if flag
	(gtb-apply-tag stream "grhigh" iter iter1)
	(gtb-remove-tag stream "grhigh" iter iter1))))


(defmethod pres-button-press ((p t) stream event))




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

;;===============================================================================
(defun make-vsldb (connection thread level conditio restarts frames continuations)
  "create the presentation, buffer and view for the debugger."
  (let* ((sldb (make-instance 
		'sldb
		:connection connection
		:thread thread
		:level level
		:conditio conditio
		:restarts restarts
		:frames frames
		:continuations continuations)))
    (make-rview sldb)))

(defun vsldb-activate (vsldb)
  (sldb-activate (gtk-text-view-buffer vsldb)))

(defun vsldb-destroy (vsldb)
  (rview-destroy-top vsldb ))

;;===============================================================================
(defclass sldb (rbuffer)
  ((connection    :accessor connection    :initarg :connection    :initform nil )
   (thread        :accessor thread        :initarg :thread        :initform nil )
   (level         :accessor level         :initarg :level         :initform nil )
   (conditio      :accessor conditio      :initarg :conditio      :initform nil )
   (restarts      :accessor restarts      :initarg :restarts      :initform nil )
   (frames        :accessor frames        :initarg :frames        :initform nil )
   (continuations :accessor continuations :initarg :continuations :initform nil ))
  (:metaclass gobject-class))


(defmethod initialize-instance :after ((sldb sldb) &key)
  (setf *pbuf* sldb);****
  (pbuf-new-tag sldb :name "grayish"     :foreground "gray"        :editable t)
  (pbuf-new-tag sldb :name "beige"       :foreground "beige"       :editable t)
  (pbuf-new-tag sldb :name "restartable" :foreground "LimeGreen"   :editable t)
  (pbuf-new-tag sldb :name "normal"      :foreground "NavajoWhite" :editable t)
  (pbuf-new-tag sldb :name "cyan"        :foreground "cyan"        :editable t)
  (pbuf-new-tag sldb :name "label"       :foreground "Gray70"      :background "Gray18" :editable nil)
  (pbuf-new-tag sldb :name "enum"        :foreground "Gray70"      :editable t)
  (pbuf-new-tag sldb :name "condition"   :foreground "plum"        :editable t)
  (pbuf-new-tag sldb :name "grhigh"      :background "darkgreen"   :foreground "NavajoWhite" :editable nil)
  (pbuf-new-tag sldb :name "grayloc:"    :background "Gray20"      :foreground "Gray70"      :editable nil )
  (pbuf-new-tag sldb :name "locleft"     :foreground "yellow"                                :editable nil))
(defmethod -on-announce-eli :after ((sldb sldb) eli)
    (with-slots (keymap) eli
    (keymap-define-key keymap #.kb:|q| (lambda (key) (sldb-quit sldb)))))




(defmethod -on-button-press ((sldb sldb) iter event)
  (mvb (range off) (range:actual (root sldb) (gti-get-offset iter))
	(pres-button-press range sldb event)))

(let (last)
  (defmethod -on-motion ((sldb sldb) iter event)
    (let ((range (range:actual (root sldb) (gti-get-offset iter))))
      ;;(bug "~A~&" range)
      (unless (eq last range)
	(pres-highlight last sldb nil)
	(pres-highlight range sldb t)
	(setf last range)))))
;;; Return a list (LOCALS TAGS) for vars and catch tags in the frame INDEX.
;;; LOCALS is a list of the form ((&key NAME ID VALUE) ...).
;;;TAGS has is a list of strings.

;; For toggling, the top line remains while the rest can disappear


(defun sldb-frame-more (sldb range)
  
  )



(defun sldb-activate (stream)
  (with-slots (conditio restarts frames continuations) stream
    (with-tag  "normal" 
      (format stream "~A~&" (first conditio)))
    (with-range stream (make-instance 'pcondition)
      (with-tag "condition"
	(format stream "~A" (second conditio))))
    (with-tag  "label" (format stream "~%Restarts:~&")) 
    
    (loop for restart in restarts
       for i from 0 do
	 (with-range stream
	     (make-instance 'prestart :id i)
	   (with-tag  "enum"   (format stream "~2d: [" i))
	   (with-tag  "cyan"   (format stream "~A" (first restart)))
	   (with-tag  "normal" (format stream "] ~A~&" (second restart)))))
    ;;-------------------------------------------------------
    (with-tag  "label" (format stream "~%Backtrace:~&"))
    (loop for frame in frames do
	 (let (pframe)
	   (with-range stream
	       (make-instance
		'pframe
		:id (first frame)
		:desc (second frame)
		:restartable (third frame))
	     (present it stream nil)
	     (setf pframe it))
	   (with-range stream
	       (make-instance 'pframex)
	     (terpri stream)
	     (setf (pframex pframe) it)))))
  
  (finish-output stream) )

;;-----------------------------------------------------------------------------
;; Protocol
(defun sldb-invoke-restart (sldb restart)
  (with-slots (connection level thread) sldb
    (swa:emacs-rex
     connection
     (format nil "(swank:invoke-nth-restart-for-emacs ~A ~A)" level restart)
     :thread thread)))

(defun sldb-quit (sldb)
  (with-slots (connection level thread) sldb
        (swa:emacs-rex connection "(swank:throw-to-toplevel)" :thread thread)))



||#
