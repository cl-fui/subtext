(in-package :stext)


(defclass sldb (rbuffer)
  ((connection    :accessor connection    :initarg :connection    :initform nil )
   (thread        :accessor thread        :initarg :thread        :initform nil )
   (level         :accessor level         :initarg :level         :initform nil )
   (conditio      :accessor conditio      :initarg :conditio      :initform nil )
   (restarts      :accessor restarts      :initarg :restarts      :initform nil )
   (frames        :accessor frames        :initarg :frames        :initform nil )
   (continuations :accessor continuations :initarg :continuations :initform nil ))
  (:metaclass gobject-class))

(defparameter *q* nil)
(defmethod initialize-instance :after ((sldb sldb) &key)
  (setf *pbuf* sldb);****

  (pres-tag sldb pcondition (:foreground "plum" :editable nil))
  (pres-tag sldb prestart   (:foreground "yellow" :editable nil))
  (pres-tag sldb pframe     ())
  
  (setf *q* (make-instance 'pcondition)   )
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

;;===============================================================================
;; Initial display
;;
(defun sldb-activate (out)
  (with-slots (conditio restarts frames continuations) out
    (format t "~A ~A ~A ~A~&" conditio restarts frames continuations)
    (with-tag "enum" (format out "~A~&" (first conditio)))
    (with-pres pcondition (format out "~A" (second conditio)))
    (with-tag  "label" (format out "~%Restarts:~&"))   
    (loop for restart in restarts
       for i from 0 do
	 (present (make-instance 'prestart :id i :info restart))
	 (terpri out))
    ;;
    (with-tag  "label" (format out "~%Backtrace:~&"))
    (loop for frame in frames do
	 (present
	  (make-instance 'pframe :id (first frame) :desc (second frame)
			 :restartable (third frame)))
	 (terpri out)))
  
  
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

;; 
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

(defmethod -pres-on-button ((pres prestart) button times pressed)
  (with-slots (id out) pres
    (with-slots (connection level thread) out
      (swa:emacs-rex
       connection
       (format nil "(swank:invoke-nth-restart-for-emacs ~A ~A)" level id)
       :thread thread))))
;;===============================================================================
;; frame
;;
;; Protocol: (4 (SWANK-REPL::TRAC...) (RESTARTABLE T))
(defpres pframe (pres)
  (id desc restartable pframex expand))

(defpresenter ((p pframe))
  (with-slots (id desc restartable pframex expand) p
    (with-tag  "enum" (format out "~3d: " id))
    (with-tag  (if restartable "restartable" "normal")
      (format out "~A" desc))))

(defmethod  -pres-on-mouse ((pres pframe) flag)
  (with-slots (out) pres
    (with-slots (iter iter1) out
      (pres-bounds out pres)
      (if flag
	  (gtb-apply-tag out "grhigh" iter iter1)
	  (gtb-remove-tag out "grhigh" iter iter1)))))
