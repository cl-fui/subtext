(in-package :subtext)


(defcontext pframe (ctx)
  (id desc restartable pframex (expanded :accessor expanded :initform nil)))
(defkeymap pframe)

(defcontext prestart (ctx)
  ((id :accessor id :initarg :id :initform 0)
   (info :accessor info :initarg :info :initform nil)))
(defkeymap prestart)


(defclass sldb (conbuf)
  ((connection    :accessor connection    :initarg :connection    :initform nil )
   (thread        :accessor thread        :initarg :thread        :initform nil )
   (level         :accessor level         :initarg :level         :initform nil )
   (conditio      :accessor conditio      :initarg :conditio      :initform nil )
   (restarts      :accessor restarts      :initarg :restarts      :initform nil )
   (frames        :accessor frames        :initarg :frames        :initform nil )
   (continuations :accessor continuations :initarg :continuations :initform nil )
   (eli           :accessor eli))
  (:metaclass gobject-class))

(defmethod initialize-instance :after ((sldb sldb) &key)
  (setf *pbuf* sldb);****
  ;; register presentation types with this buffer and tags
  (context-tag sldb pcondition (:foreground "plum" :editable nil))
  (context-tag sldb prestart   (:foreground "yellow" :editable nil))
  (context-tag sldb pframe     ())
  (context-tag sldb pframex    ())
  
  
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

(defmethod -on-announce-eli ((sldb sldb) eli)
  (with-slots (connection thread level) sldb
    (setf (eli sldb) eli)
    (eli-def eli (kbd "q")
	     (lambda (subtext context)
	       (swa:emacs-rex connection  "(swank:throw-to-toplevel)"
			      :thread thread)))
    (eli-def eli (kbd "a")
	     (lambda (subtext context)
	       (swa:emacs-rex connection  "(swank:sldb-abort)"
			      :thread thread)))
)
					;  (clrf keymap-prestart keymap-pframe)
  (setf keymap-prestart nil)
  (setf keymap-pframe nil)
  
  (keymap-def keymap-prestart (kbd "Mouse-1")
	      (lambda (subtext prestart)
		(sldb-invoke-restart subtext (id prestart))
		nil))

  (keymap-def keymap-pframe (kbd "Mouse-1")
	      (lambda (subtext pframe)
		(with-slots (expanded pframex) pframe
		  (setf expanded (pframex-toggle subtext pframex expanded)))
		nil)) )

(defun sldb-invoke-restart (sldb id)
  (with-slots (connection level thread) sldb
    (swa:emacs-rex
     connection
     (format nil "(swank:invoke-nth-restart-for-emacs ~A ~A)" level id)
     :thread thread)))

;;===============================================================================
;; Initial display
;;
(defun sldb-activate (out)
  (with-slots (eli conditio restarts frames continuations) out
 ;;   (format t "~A ~A ~A ~A~&" conditio restarts frames continuations)
    (with-tag ("enum" out) (format out "~A~&" (first conditio)))
    (with-context (pcondition) (format out "~A" (second conditio)))
    (with-tag  ("label" out) (format out "~%Restarts:~&"))   
    (loop for restart in restarts
       for i from 0 do
	 (when (< i 10) ;bind numeric keys
	   (eli-def eli (cons (+ i #x30) nil)
		    (lambda (subtext context) (sldb-invoke-restart out i))))
	 (present (make-instance 'prestart :id i :info restart))
	 (terpri out))
    ;;
    (with-tag ("label" out) (format out "~%Backtrace:~&"))
    (loop for frame in frames do
	 (present
	  (make-instance 'pframe :id (first frame) :desc (second frame)
			 :restartable (third frame)))))
  (finish-output out) )
;;===============================================================================
;; Here is the view
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

;;===============================================================================
;;
(defcontext pcondition (ctx) () )

;;===============================================================================
;; Restart
;;
;; Protocol: (RETRY "Retry SLIME REPL evaluation request.")
;;

;; defpresenter is a convenience method, 
(defpresenter ((p prestart))
  (with-slots (id info) p
    (with-tag  ("enum" out)   (format out "~2d: [" id))
    (with-tag  ("cyan" out)   (format out "~A" (first info)))
    (with-tag  ("normal" out) (format out "] ~A" (second info)))))

(defmethod  -con-mouse-enter (subtext (context prestart) flag)
  (with-subtext subtext
    (with-context-bounds context
      (tag-apply "grhigh"))))

(defmethod  -con-mouse-exit (subtext (context prestart) flag)
  (with-subtext subtext
    (with-context-bounds context
      (tag-remove "grhigh"))))



;;===============================================================================
;; framex - expanded frame...
;;
;; Exceptionally weird: starts out as a CR; when frame is pressed, it toggles.
;; between cr and a frame description obtained from the server.
(defcontext pframex (ctx) (id))
(defpresenter ((p pframex))
  (terpri (out p))) ;initial presentation is a lone CR!
;;
;; Note: it seems that tags have a right-gravity...
;;
(defun pframex-toggle (stream pframex expanded)
  (context-bounds stream pframex)
  (if expanded
      (with-slots (iter iter1) stream
	(gti-backward-char iter1);leave the cr
	(format t "DELETING ~A ~A~&" iter iter1)
	(%gtb-delete stream iter iter1)
	nil)
      (let ((out stream))
	(with-slots (connection thread) stream
	  (with-slots (id) pframex
	    (swa:emacs-rex
	     connection
	     (format nil "(swank:frame-locals-and-catch-tags ~A)" id)
	     :thread thread
	     :proc
	     (lambda (connection reply id)
	       (gsafe
		(context-bounds stream pframex); it is much later, in lambda!
		(file-position stream (gti-offset (iter1 stream)))
		(format t "TAG-FRAMEX ~A~&" (tag pframex))
		(with-tag ((tag pframex) stream)
		  (with-tag ("grayloc:" stream)
		    (princ "Locals:" stream))
		  (loop for item in (first (second reply)) do
		       (terpri stream)
		       (princ "      " stream)
		       (with-tag ("locleft" stream) (princ (second item) stream))
		       (with-tag ("normal" stream)  (princ " = "  stream))
		       (with-tag ("pres" stream)    (princ (sixth item) stream)))
		  (terpri stream); make sure pres ends with CR
		  )
		(finish-output stream))))))
	
	t)))
;;===============================================================================
;; 
;;
;; Protocol: (4 (SWANK-REPL::TRAC...) (RESTARTABLE T))

(defpresenter ((p pframe))
  (with-slots (id desc restartable pframex) p
    (with-tag  ("enum" out) (format out "~3d: " id))
    (with-tag  ((if restartable "restartable" "normal") out)
      (format out "~A" desc))))

;; after the presentation, outside of it, create a hidden presentation
;; give it our id, so it can expand
(defmethod present :after ((p pframe))
  (with-slots (id pframex ) p
    (present (setf pframex (make-instance 'pframex :id id)))))


;;------------------------------------------------------------------------------
;; mouse
(defmethod  -con-mouse-enter (subtext (context pframe) level)
  (with-subtext subtext
    (with-context-bounds context
      (tag-apply "grhigh"))))

(defmethod  -con-mouse-exit (subtext (context pframe) level)
  (with-subtext subtext
    (with-context-bounds context
      (tag-remove "grhigh"))))



;;------------------------------------------------------------------------------

