(in-package :stext)
;;------------------------------------------------------------------------------
;; gtbstream
;;
;; output at the end only!

(defclass termstream (gtk-text-buffer
		      trivial-gray-streams:fundamental-character-output-stream)
  
  ((iter         :accessor iter   :initform nil :type gtk-text-iter)
   (iter1        :accessor iter1  :initform nil :type gtk-text-iter)
   (markin       :accessor markin :initform nil :type gtk-text-mark)

   (lbuf         :accessor lbuf   :initform (make-array 4096 :element-type 'character))
   (index        :accessor index  :initform 0   :type fixnum)
   (promises     :accessor promises :initform nil)
   (active-range :accessor active-range :initform nil ))
  
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream termstream) &key)
 ;; (print "init-inst TERMSTREAM")
  (with-slots (active-range iter iter1 markin root) stream
    (setf iter   (gtb-get-start-iter stream)
	  iter1  (gtb-get-end-iter   stream)
	  markin (gtb-create-mark stream (null-pointer) iter t)
	  active-range (range:make))))
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream termstream))
  (with-slots (markin) stream
    (%gtb-delete-mark stream markin)))



;;==============================================================================
;;
(defmethod trivial-gray-streams:stream-write-char ((stream termstream) char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-emit stream char))
;;------------------------------------------------------------------------------
(defmethod trivial-gray-streams:stream-line-column ((stream termstream))
  ;;(format t "line-col~&")
  (stream-flush stream)
  (gti-get-line-offset (iter stream)))

(defmethod trivial-gray-streams:stream-start-line-p ((stream termstream))
  ;;(format t "startlinep~~&")
  (stream-flush stream)
  (gti-starts-line (iter stream)))
		;;------------------------------------------------------------------------------


(defmethod trivial-gray-streams:stream-force-output ((stream termstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(format t "Force")
  (stream-flush stream))
(defmethod trivial-gray-streams:stream-finish-output ((stream termstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(format t "Finish~~&")
  (stream-flush stream)
  )

(defmethod trivial-gray-streams:stream-file-position ((stream termstream))
  (stream-position stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream termstream))
  (stream-flush stream)
  nil)
;;==============================================================================
(defun stream-emit (stream char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type character char))
  (with-slots (index lbuf active-range) stream
    "Emit a character into stream.  May cache until..."  
;;    (format t "~%Emitting ~C into ~A at ~A" char stream index)
;;    (print (range:childest active-range))
    (setf (schar lbuf index) char)
    (incf (the fixnum index))
    (range::widen-prim (range:childest active-range) 1 )
    (when (> (the fixnum index) 508);
      (stream-flush stream))
    char))
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
(defun stream-flush (stream)
  "Flush the stream if needed; return offset"
  (declare (optimize (speed 3) (safety 0) (debug 3)))
  
  (with-slots (iter index lbuf promises) stream
;;    (format t "Flushing ~A characters~&" index)
;;    (print (root stream))
    (unless (zerop (the fixnum index))
      (setf (schar lbuf (the fixnum index)) #\Nul ;terminate UTF8 lbuf
	    index 0)
      (%gtb-get-end-iter stream iter)
      (%gtb-insert stream iter lbuf -1)
      (when promises (term-promises stream)))))
;;==============================================================================
;; And our extensions...
(declaim (inline stream-real-position))
(defun stream-position (stream)
  (declare (optimize (speed 3) (safety 0) (debug 3)))
  (the fixnum (+ (the fixnum (gtb-get-char-count stream))
		 (the fixnum (index stream)))))

(defun stream-wipe (stream)
  (with-slots (index iter iter1 promises) stream
    (%gtb-get-start-iter stream iter)
    (%gtb-get-end-iter   stream iter1)
    (%gtb-delete stream iter iter1)
    (setf index 0
	  promises nil)))

(defun stream-apply-tag (stream tag start end)
  (with-slots (iter iter1) stream
    (%gtb-get-iter-at-offset stream iter start)
    (%gtb-get-iter-at-offset stream iter1 end)
    (gtb-apply-tag stream tag iter iter1))
)
;;==============================================================================
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Promises
;;
;; A promise is a promise to perform some task on the buffer in the future. A
;; promise has start and end offsets, and the content, such as tag..
;; We keep offsets as numeric offsets.  That should mostly work as insertion
;; happes at the end.  Should we make a promise, then insert in the beginning
;; of a buffer, bad things will happen TODO~
;;
;; Promises are made with a (promising "bold" whatever) forms.  At the end of
;; the promising form, the promise is placed on the promises list.  When the
;; output is finished,  promises are resolved.  
(defstruct promise start end content)

(defparameter promise-free-list nil)
(loop for i from 0 to 200 do (push (make-promise :start 0 :end 0 :content nil) promise-free-list))
(defun promise-new (&key (start 0) (end 0) (content nil))
  (let ((promise (pop promise-free-list)))
    (setf (promise-start promise) start
	  (promise-end promise) end
	  (promise-content promise) content)
    promise))

(defun promise-free (promise)
  (setf (promise-content promise) nil)
  (push promise promise-free-list))

(defmethod promise-in (stream (content t))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type termstream stream))
  (promise-new :start (stream-position stream)
		:content content))

(defmethod promise-out (stream promise (content t))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type termstream stream)
	   (type promise promise))
  (setf (promise-end promise) (stream-position stream))
  (push promise (promises stream)))

;; this macro requires stream to be called stream!
;; injects it for the thing promised...

(defmacro with-tag (tag &body body)
  (let ((promise (gensym)))
    `(let* ((it ,tag)		    ;anaphoric it for the presentation
	    (,promise (promise-in stream it)))
       ,@body
       (promise-out stream ,promise it))))

(defmethod promise-fulfill ((tag gtk-text-tag) promise stream)
  (with-slots (start end) promise
    (with-slots (iter iter1) stream
      (%gtb-get-iter-at-offset stream iter start)
      (%gtb-get-iter-at-offset stream iter1 end)
      (%gtb-apply-tag stream tag iter iter1))))

(defmethod promise-fulfill ((tag string)  promise stream )
  (with-slots (start end) promise
    (with-slots (iter iter1) stream
      (%gtb-get-iter-at-offset stream iter start)
      (%gtb-get-iter-at-offset stream iter1 end)
      (gtb-apply-tag-by-name stream tag iter iter1))))

(defmethod promise-fulfill ((range range:range) promise stream)
  (format t "fulfilling range...~A start ~A end ~A-~&" range	  (promise-start promise) (promise-end promise))
  (format t "rpad is ~A~&" (- (stream-position stream) (promise-end promise)))
  (format t "range ~A, ~A~&" range  (range:kids range))
  (range:sub range (- (stream-position stream) (promise-end promise)))
  )
 


(defun term-promises (stream)
;;  (print (promises stream))
;; (print "---------------")
;;  ;; for each promise, fullfil it
;;  (print (root stream))
  (with-slots (promises iter iter1) stream
    ;; reverse is important: ranges must fill left to right
    (loop for promise in (reverse (promises stream)) do
	 (promise-fulfill (promise-content promise) promise stream))
    (loop for promise in (promises stream) do
	 (promise-free promise)))
  
  (setf (promises stream) nil))
;;==============================================================================
;; Range-in
;;
;; Create a range under active range and switch to it.
;;
(defun range-in (stream range)
  (with-slots (active-range) stream
    
    (setf (range:child active-range) range ;we are active's child!
	  (range:dad range) active-range   ;like so
	  ;; attach a left pad, so that range:at works... TODO:waste!
	  (range:l   range) (range:make :width (range:width active-range)
					:dad active-range)
	  active-range range)))
;;
;; To get out, we set active range to our dad.  But, we must create a pad below
;; next to the old range.  As we expand the dad, the pad will keep the old
;; range intact.

(defun range-out (stream)
  (with-slots (active-range) stream
    ;; create a pad next to active-range to ensure that it stays where it should
    (let ((pad (range:make :dad (range:dad active-range)
			   :l active-range)))
      
      (if (range:dad (range:dad active-range));; this is how we bottom out!
	  (setf active-range (range:dad active-range)
		(range:child active-range) pad)
	  (progn
	    (let* ((here (stream-position stream))
		   (promise
		    (promise-new :content active-range
				  :end here 
				  :start (- here (range:width active-range)))))
	      (setf (range:dad active-range) (root stream))
	     ;;
;;	      (format t "Promising: ~A start:~A end:~A ~&"	      active-range (promise-start promise)(promise-end promise))
	      ;;(print active-range)
	      (push promise (promises stream))
	      ;;might as well recycle the pad
	      (setf (range:dad pad) nil
		    (range:l pad) nil
		    active-range pad))
	    ))
      )))

(defmacro with-range (stream range &body body)
  `(let ((it ,range))
     (range-in ,stream it)
     ,@body
     (range-out ,stream)
     ))
;;------------------------------------------------------------------------------
;;


;;==============================================================================
;; Simple-Input support
;; A very simple interface to 
(defun simple-input-mark (stream)
  ;; Keep a marker as start of input.
  "Set input mark to end of buffer after flushing."
  (with-slots (iter markin) stream
    (stream-flush stream)
    (%gtb-get-end-iter stream iter)
    (%gtb-move-mark stream markin iter)
    ))

(defun simple-input-get-text (stream)
  "get text from markin to end"
  (with-slots (iter iter1 markin) stream 
    (simple-input-iters stream) 
    (gtb-get-text stream iter iter1 nil)))

(defun simple-input-iters (stream)
  "set iters to start and end"
  (with-slots (iter iter1 markin) stream
    (stream-flush stream)
    (%gtb-get-iter-at-mark stream iter markin)
    (%gtb-get-end-iter stream iter1))
)

(defun simple-input-promise (stream content)
  "make a promise on a range of last input"
  (with-slots (iter iter1 markin promises) stream
    (simple-input-iters stream)
    (push 
     (promise-new :start (gti-offset iter)
		  :end   (gti-offset iter1)
		  :content content)
     promises)))
