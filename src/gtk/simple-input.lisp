(in-package :subtext)
;;------------------------------------------------------------------------------
;; Markstream
;;
;; An independent-of-buffer stream using mark, caching output, promises.
;;
(defclass simple-input ()
  ((buffer     :accessor buffer   :initarg :buffer :initform nil)
   (iter       :accessor iter     :initform nil :type gtk-text-iter)
   (iter1      :accessor iter1    :initform nil :type gtk-text-iter)
   (mark       :accessor mark :initform nil :type gtk-text-mark)))

;;==============================================================================
(defmethod initialize-instance :after ((stream simple-input) &key)
  (with-slots (buffer iter iter1 mark) stream
    (setf iter (gtb-get-end-iter buffer)
	  iter1 (gtb-get-end-iter buffer))
    (setf mark (gtb-create-mark buffer (null-pointer) iter t))))
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream simple-input))
  (with-slots (buffer mark) stream
    (%gtb-delete-mark buffer mark)))

(defun simple-input-iters (stream)
  "set iters to start and end"
  (with-slots (buffer iter iter1 mark) stream
    (%gtb-get-iter-at-mark buffer iter mark)
    (%gtb-get-end-iter buffer iter1))
  )
(defun simple-input-mark (stream)
  ;; Keep a marker as start of input.
  "Set input mark to end of buffer after flushing."
  (with-slots (buffer iter mark) stream
    (%gtb-get-end-iter buffer iter) ;in this case, really want an end iter!
    (%gtb-move-mark buffer mark iter)))

(defun simple-input-get-text (stream)
  "get text from mark to end"
  (with-slots (buffer iter iter1 mark) stream 
    (simple-input-iters stream) 
    (gtb-get-text buffer iter iter1 nil)))


;;----------------------------------------------------------------------
;; A promise
(defun simple-input-promise (stream content)
  "make a promise on a range of last input"
  (with-slots (buffer iter iter1) stream
    (simple-input-iters stream)
    (push (make-promise  :start (gti-offset iter)
			 :end (gti-offset iter1)
			 :content content)
	  (promises buffer))))


