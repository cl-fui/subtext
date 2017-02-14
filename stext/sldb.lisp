(in-package :stext)


(defclass sldb (rbuffer)
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
  (pres-tag sldb pcondition (:foreground "plum" :editable nil))
  (pres-tag sldb prestart   (:foreground "yellow" :editable nil))
  (pres-tag sldb pframe     ())
  (pres-tag sldb pframex    ())
  
  
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
    (eli-def eli (kbd "Mouse-1")
	     (lambda ();; (format t "~A ~A~&" (x eli) (y eli))
	       (with-slots (x y) eli
		 (let* ((iter (rview-iter-from-xy eli x y))
			(presentations (presentations-at sldb iter)))
		   (loop for pres in presentations
		      until (-pres-on-button pres 1))))))
    (eli-def eli (kbd "q")
	     (lambda ()
	       (swa:emacs-rex connection  "(swank:throw-to-toplevel)"
			      :thread thread)))
    (eli-def eli (kbd "a")
	     (lambda ()
	       (swa:emacs-rex connection  "(swank:sldb-abort)"
			      :thread thread))))
  
  )

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
    (with-tag "enum" (format out "~A~&" (first conditio)))
    (with-pres pcondition (format out "~A" (second conditio)))
    (with-tag  "label" (format out "~%Restarts:~&"))   
    (loop for restart in restarts
       for i from 0 do
	 (when (< i 10) ;bind numeric keys
	   (eli-def eli (cons (+ i #x30) nil) (lambda () (sldb-invoke-restart out i))))
	 (present (make-instance 'prestart :id i :info restart))
	 (terpri out))
    ;;
    (with-tag  "label" (format out "~%Backtrace:~&"))
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

(defun vsldb-destroy (vsldb)
  (rview-destroy-top vsldb ))
;;===============================================================================
;;
(defpres pcondition (pres) () )

;;===============================================================================
;; Restart
;;
;; Protocol: (RETRY "Retry SLIME REPL evaluation request.")
;;
(defpres prestart (pres)
  ((id :accessor id :initarg :id :initform 0)
   (info :accessor info :initarg :info :initform nil)))

;; defpresenter is a convenience method, 
(defpresenter ((p prestart))
  (with-slots (id info) p
    (with-tag  "enum"   (format out "~2d: [" id))
    (with-tag  "cyan"   (format out "~A" (first info)))
    (with-tag  "normal" (format out "] ~A" (second info)))))

(defmethod  -pres-on-mouse ((pres prestart) flag)
  (with-slots (out) pres
    (with-slots (iter iter1) out
      (pres-bounds out pres)
      (if flag
	  (gtb-apply-tag out "grhigh" iter iter1)
	  (gtb-remove-tag out "grhigh" iter iter1)))))



(defmethod -pres-on-button ((pres prestart) button )
  (with-slots (id (sldb out)) pres
    (sldb-invoke-restart sldb id)))

;;===============================================================================
;; framex - expanded frame...
;;
;; Exceptionally weird: starts out as a CR; when frame is pressed, it toggles.
;; between cr and a frame description obtained from the server.
(defpres pframex (pres) (id))
(defpresenter ((p pframex))
  (terpri (out p))) ;initial presentation is a lone CR!
;;
;; Note: it seems that tags have a right-gravity...
;;
(defun pframex-toggle (stream pframex expanded)
  (pres-bounds stream pframex)
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
		(pres-bounds stream pframex); it is much later, in lambda!
		(file-position stream (gti-offset (iter1 stream)))
		(format t "TAG-FRAMEX ~A~&" (tag pframex))
		(with-tag (tag pframex)
		  (with-tag "grayloc:"
		    (princ "Locals:" stream))
		  (loop for item in (first (second reply)) do
		       (terpri stream)
		       (princ "      " stream)
		       (with-tag "locleft" (princ (second item) stream))
		       (with-tag "normal"  (princ " = "  stream))
		       (with-tag "pres"    (princ (sixth item) stream)))
		  (terpri stream); make sure pres ends with CR
		  )
		(finish-output stream))))))
	
	t)))
;;===============================================================================
;; frame
;;
;; Protocol: (4 (SWANK-REPL::TRAC...) (RESTARTABLE T))
(defpres pframe (pres)
  (id desc restartable pframex (expanded :accessor expanded :initform nil)))

(defpresenter ((p pframe))
  (with-slots (id desc restartable pframex) p
    (with-tag  "enum" (format out "~3d: " id))
    (with-tag  (if restartable "restartable" "normal")
      (format out "~A" desc))))

;; after the presentation, outside of it, create a hidden presentation
;; give it our id, so it can expand
(defmethod present :after ((p pframe))
  (with-slots (id pframex ) p
    (present (setf pframex (make-instance 'pframex :id id)))))


;;------------------------------------------------------------------------------
;; mouse
(defmethod  -pres-on-mouse ((pres pframe) flag)
  (with-slots (out) pres
    (with-slots (iter iter1) out
      (pres-bounds out pres)
      (if flag
	  (gtb-apply-tag out "grhigh" iter iter1)
	  (gtb-remove-tag out "grhigh" iter iter1)))))
;;------------------------------------------------------------------------------
(defmethod -pres-on-button ((pres pframe) button)
  (with-slots (out expanded pframex) pres
    (setf expanded (pframex-toggle out pframex expanded)))
  t)
