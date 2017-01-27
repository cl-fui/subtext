(in-package :stext)

(defstruct (pcondition (:include range:range)) )
(defstruct (prestart   (:include range:range)) id)
(defstruct (pframe     (:include range:range)) id open)
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
    )

  )
(defclass sldb (rbuffer)
  ((connection  :accessor connection   :initarg :connection )
   (sldb-thread :accessor sldb-thread  :initarg :thread)
   (sldb-level  :accessor sldb-level   :initarg :level)
   (sldb-condition   :accessor sldb-condition    :initarg :condition)
   (sldb-restarts    :accessor sldb-restarts     :initarg :restarts)
   (sldb-frames      :accessor sldb-frames       :initarg :frames)
   (sldb-continuations :accessor sldb-continuations :initarg :continuations)
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
  (pbuf-new-tag sldb :name "condition" :foreground "plum"  :editable nil))

(defmethod -on-announce-eli :after ((sldb sldb) eli)
  (setf (sldb-eli sldb) eli)
  (with-slots (keymap) eli
    (keymap-bind keymap "0" (lambda () (sldb-invoke-restart sldb 0)))
    (keymap-bind keymap "1" (lambda () (sldb-invoke-restart sldb 1)))
    (keymap-bind keymap "2" (lambda () (sldb-invoke-restart sldb 2)))
    (keymap-bind keymap "3" (lambda () (sldb-invoke-restart sldb 3)))
    (keymap-bind keymap "4" (lambda () (sldb-invoke-restart sldb 4)))
    (keymap-bind keymap "5" (lambda () (sldb-invoke-restart sldb 5)))
    (keymap-bind keymap "6" (lambda () (sldb-invoke-restart sldb 6)))
    (keymap-bind keymap "7" (lambda () (sldb-invoke-restart sldb 7)))
    (keymap-bind keymap "8" (lambda () (sldb-invoke-restart sldb 8)))
    (keymap-bind keymap "9" (lambda () (sldb-invoke-restart sldb 9)))
    (keymap-bind keymap "q" (lambda () (sldb-quit sldb))))
  )

(defmethod -on-button-press ((sldb sldb) view event)
  (let ((iter (rview-iter-from-event view event)))
    (mvb (range off) (range:at (root sldb) (gti-get-offset iter))
	 (typecase range
	   (prestart (sldb-invoke-restart sldb (prestart-id range)))
	   (pframe (sldb-frame-toggle sldb range))))))

;;; Return a list (LOCALS TAGS) for vars and catch tags in the frame INDEX.
;;; LOCALS is a list of the form ((&key NAME ID VALUE) ...).
;;;TAGS has is a list of strings.
(defun sldb-frame-toggle (sldb range)
  (with-slots (connection sldb-thread) sldb
    (with-slots (id open) range
      (swa:emacs-rex
       connection
       (format nil "(swank:frame-locals-and-catch-tags ~A)" id)
       :thread sldb-thread
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
		))))))
  )
(defun make-wsldb (connection thread level condition restarts frames continuations)
  (let* ((sldb (make-instance
		'sldb :connection connection :thread thread :level level
		:condition condition :restarts restarts :frames frames
		:continuations continuations)))
    (make-wtxt sldb)))

(defun wsldb-activate (wsldb)
  (sldb-activate (gtk-text-view-buffer wsldb)))

(defun wsldb-destroy (wsldb)
  (let ((frame (frame (sldb-eli (gtk-text-view-buffer wsldb)))))
    (gtk-widget-destroy frame)
    )
  
  )
(defun sldb-activate (sldb)
  (with-slots (sldb-condition sldb-restarts sldb-frames sldb-continuations) sldb
   
    (with-tagname sldb "normal" 
      (format sldb "~A~&" (first sldb-condition)))
    (stream-delimit sldb (make-pcondition))
    (with-tagname sldb "condition"
      (format sldb "~A~&" (second sldb-condition)))
    (stream-delimit sldb nil)
    
    (with-tagname sldb "label" (format sldb "~%Restarts:~&"))
    (loop for restart in sldb-restarts
       for i from 0 do
	 (stream-delimit sldb (make-prestart :id i))
	 (with-tagname sldb "enum"   (format sldb "~2d: [" i))
	 (with-tagname sldb "cyan"   (format sldb "~A" (first restart)))
	 (with-tagname sldb "normal" (format sldb "] ~A~&" (second restart)))
	 (stream-delimit sldb nil))
    (with-tagname sldb "label" (format sldb "~%Backtrace:~&"))
    (loop for frame in sldb-frames
       for i from 0 do
	 (stream-delimit sldb (make-pframe :id i) )
	 (with-tagname sldb "enum"
	   (format sldb "~3d: "  (first frame)))
	 (with-tagname sldb (if (third frame) "restartable" "normal")
	   (format sldb "~A~&"   (second frame)))
	 (stream-delimit sldb nil))))


(defun sldb-invoke-restart (sldb restart)
  (with-slots (connection sldb-level sldb-thread) sldb
    (swa:emacs-rex
     connection
     (format nil "(swank:invoke-nth-restart-for-emacs ~A ~A)" sldb-level restart)
     :thread sldb-thread)))

(defun sldb-quit (sldb)
  (with-slots (connection sldb-thread) sldb
    (swa:emacs-rex connection "(swank:throw-to-toplevel)" :thread sldb-thread)))



