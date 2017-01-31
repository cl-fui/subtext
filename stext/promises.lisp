(in-package :stext)
;;------------------------------------------------------------------------------
;;==============================================================================
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Promises
;;
;; A promise is a contract to perform some task on the buffer in the future. A
;; promise has start and end offsets, and the content, such as tag..
;; We keep offsets as numeric offsets.  That should mostly work as insertion
;; happes at the end.  Should we make a promise, then insert in the beginning
;; of a buffer, bad things will happen TODO~
;;
;; Promises are made with a (promising "bold" whatever) forms.  At the end of
;; the promising form, the promise is placed on the promises list.  When the
;; output is finished,  promises are resolved.  
(defstruct promise start end content)

;; Out of desperation, I am keeping a pool of promises to avoid consisng...
(defun promise-new (stream &key (start 0) (end 0) (content nil))
  (with-slots (promise-free-list) stream
    (let ((promise (pop promise-free-list)))
      (setf (promise-start promise) start
	    (promise-end promise) end
	    (promise-content promise) content)
      promise)))

(defun promise-free (stream promise)
  (with-slots (promise-free-list) stream
    (setf (promise-content promise) nil)
    (push promise promise-free-list)))

(defun promises-free (stream promises)
  (with-slots (promise-free-list) stream
    (loop for promise in promises do
	 (promise-free stream promise))))
;;------------------------------------------------------------------------------
;; Called by with-tag macro.
(defmethod tag-in (stream (content t))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type termstream stream))
  (make-promise :start (stream-position stream)
		:content content))

(defun tag-out (stream promise)
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
	    (,promise (tag-in stream it)))
       ,@body
       (tag-out stream ,promise))))


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
;;  (format t "fulfilling range...~A start ~A end ~A-~&" range	  (promise-start promise) (promise-end promise))
;;  (format t "rpad is ~A~&" (- (stream-position stream) (promise-end promise)))
;;  (format t "range ~A, ~A~&" range  (range:kids range))
  (range:sub range (- (stream-position stream) (promise-end promise)))
  )
 


(defun promises-fulfill (stream)
;;  (print (promises stream))
;; (print "---------------")
;;  ;; for each promise, fullfil it
;;  (print (root stream))
  (with-slots (promises iter iter1) stream
    ;; reverse is important: ranges must fill left to right
    (loop for promise in (reverse promises) do
	 (promise-fulfill (promise-content promise) promise stream))
    ;(promises-free stream promises)
    )
    (setf (promises stream) nil))
;;==============================================================================
;; Range-in
;;
;; Create a range under active range and switch to it.
;;
(defun range-in (stream range)
  (with-slots (active-range) stream
    
    ;;(range:display active-range)
    (let ((old (range:child active-range)))
      (setf (range:child active-range) range ;we are active's child!
	    (range:dad range) active-range   ;like so
	    ;; attach a left pad, so that range:at works... TODO:waste!
	    (range:l range) (if old old
				(range:make :width (range:width active-range)
					    :dad active-range))
	    active-range range))))
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
		    (make-promise :content active-range
				 :end here 
				 :start (- here (range:width active-range)))))
	      (setf (range:dad active-range) (root stream))
	      (push promise (promises stream))
	      ;;might as well recycle the pad
	      (setf (range:dad pad) nil
		    (range:l pad) nil
		    active-range pad))
)))))

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
    (%gtb-get-end-iter stream iter) ;in this case, really want an end iter!
    (%gtb-move-mark stream markin iter)))

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
     (make-promise 
		  :start (gti-offset iter)
		  :end   (gti-offset iter1)
		  :content content)
     promises)))
