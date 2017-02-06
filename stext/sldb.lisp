(in-package :stext)

(defpres pcondition (pres) :tag (:foreground "plumb"))

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

(defparameter *q* nil)
(defmethod initialize-instance :after ((sldb sldb) &key)
  (setf *pbuf* sldb);****

  (pbuf-pres-classes sldb '(pcondition) )
  (setf *q* (make-instance 'pcondition)
   )
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


(defun sldb-activate (stream)
  (with-slots (conditio restarts frames continuations) stream
    ;;(format t "~A ~A ~A ~A~&" conditio restarts frames continuations)
    (with-tag "enum" (format stream "~A~&" (first conditio)))
    (with-pres pcondition (format stream "~A" (second conditio))))
  
  (finish-output stream) )
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



