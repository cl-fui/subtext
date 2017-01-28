(in-package :stext)
;;------------------------------------------------------------------------------
;; gtbstream
;;
;; A thin layer around gtk-text-buffer, allowing us to treat it like a stream
;; using a marker as point

(defclass gtbstream (gtk-text-buffer
		     trivial-gray-streams:fundamental-character-output-stream)
  
  ((iter         :accessor iter  :initform nil :type gtk-text-iter)
   (iter1        :accessor iter1  :initform nil :type gtk-text-iter)
   (offset       :accessor offset :initform 0 :type fixnum)
   ;;local functions to buffer output

   (lbuf         :accessor lbuf   :initform (make-array 4096 :element-type 'character))
   (index       :accessor index  :initform 0)
   (promises    :accessor promises :initform nil) ;a list of range promises
   (range       :accessor range     :initform nil) ;current parent range
   (rangebase   :accessor rangebase :initform 0))  ;left edge of current range
  
  
  (:metaclass gobject-class))
;;==============================================================================
(defmethod initialize-instance :after ((stream gtbstream) &key)
 ;; (print "init-inst GTBSTREAM")
  (with-slots ( iter iter1) stream
    (setf iter  (gtb-get-start-iter stream)
	  iter1 (gtb-get-end-iter stream)
	  ;point (gtb-create-mark stream (null-pointer) iter nil)
	  )))
;;------------------------------------------------------------------------------
(defmethod -on-destroy :before ((stream gtbstream))
  ;;(gtb-delete-mark stream (point stream))
  )

(defmacro iter-to-offset ()
  "Set iter to point, and return it"
  `(progn (setf (gtk::gtk-text-iter-offset iter) offset) iter))

	  ;;==============================================================================
	  ;;



(defmethod trivial-gray-streams:stream-write-char ((stream gtbstream) char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-emit stream char))
		;;------------------------------------------------------------------------------

#||
(defmethod trivial-gray-streams:stream-line-column ((stream gtbstream))
  (with-slots (iter offset index) stream
    (let ((actual (+ offset index)))
      (- (gti-get-line-offset (iter-to-offset)) offset))))

(defmethod trivial-gray-streams:stream-start-line-p ((stream gtbstream))
  (with-slots (iter offset) stream
    (stream-flush stream)
    (gti-starts-line (iter-to-offset))))
		;;------------------------------------------------------------------------------

||#
(defmethod trivial-gray-streams:stream-force-output ((stream gtbstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-flush stream))
(defmethod trivial-gray-streams:stream-finish-output ((stream gtbstream))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (stream-flush stream))

(defmethod trivial-gray-streams:stream-file-position ((stream gtbstream))
  (+ (offset stream) (index stream)))

(defmethod (setf trivial-gray-streams:stream-file-position) (value (stream gtbstream))
  (stream-flush stream)
  (block main
    (with-slots (iter offset) stream
      (typecase value
	(keyword (case value
		   (:start  (setf offset 0))
		   (:end    (setf offset (gtb-get-char-count stream)))
		   (t (return-from main nil))))
	(number (setf offset value))
	(t (return-from main nil)))
      t)))

(defun stream-to-iter (stream iter)
  (stream-flush stream)
  (setf (offset stream) (gti-get-offset iter))
)
		;;==============================================================================
		;; buffer routines create a closure with a buffer... These routines get hammered
		;; and really do need to be fast; a closure seems to be a little better on SBCL
		;; So here we initialize a closure, compile emit, clear and flush and set the
		;; 3 function pointers in the stream.
		;; Notes:
		;; - keep an eye on how UTF8 characters are handled!
		;; - this can be sped up with a foreign buffer as long as UTF8 is made to work.




(defun stream-emit (stream char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type character char))
  (with-slots (index lbuf) stream
    "Emit a character into stream.  May cache until..."
    
    ;;	    (format t "~%Emitting ~C into ~A at ~A" char stream index)
    (setf (schar lbuf index) char)
    (incf (the fixnum index))
    (when (> (the fixnum index) 508);
      (stream-flush stream)
      char)))
		;;--------------------------------------------------------------------------
		;;--------------------------------------------------------------------------



(defun stream-flush (stream)
  "Flush the stream if needed; return iter"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (iter offset index lbuf) stream
    ;;(format t "Flushing ~A characters~&" index)
    (unless (zerop (the fixnum index))
      (iter-to-offset)
      (incf (the fixnum offset) (the fixnum index))
      (setf (schar lbuf index) #\Nul ;terminate UTF8 lbuf
	    index 0) 
      (%gtb-insert stream iter lbuf -1))
    (stream-range-promises stream)
    offset))
		;;==============================================================================
		;; And our extensions...



(defun stream-wipe (stream)
  "clear the buffer entirely"
  (%gtb-delete stream
	       (gtb-get-start-iter stream)
	       (gtb-get-end-iter stream))
  (setf (index stream) 0))


(defun tagname-to-here (stream tagname start-offset)
  "apply a tag from start-offset to current position"
  (with-slots (iter iter1) stream
    (stream-flush stream) ;iter now set to end
    (%gtb-get-iter-at-offset stream iter1 start-offset);; setting iter's offset does not work
    (gtb-apply-tag stream tagname iter1 iter)))

(defun tag-to-here (stream tag start-offset)
  "apply a tag from start-offset to current position"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (iter iter1) stream
    (stream-flush stream) ;iter now set to end
    (%gtb-get-iter-at-offset stream iter1 start-offset)
    (%gtb-apply-tag stream tag iter iter1)))

		;;==============================================================================
		;;------------------------------------------------------------------------------
		;;------------------------------------------------------------------------------
		;; Tagged output
		;;
		;; As we allow nested tagged output, 'with-tags' keeps track of the tags and
		;; starting position on the stack.  The usual unwind-protect is not necessary
		;; as we do not reserve any resources
		;;





(defmacro with-tagname (stream tag &body body)
  (let* ((str (gensym))
	 (off (gensym))
	 (tg  (gensym)))
    `(let* ((,str (the gtbstream ,stream)) 
	    (,off (stream-flush ,str))
	    (,tg ,tag))
       ,@body
       (tagname-to-here ,str ,tg ,off))))






(defun append-presentation (stream dad pres)
  (declare (type gtbstream stream)
	   (type range:range dad))
  
  (stream-flush stream)
  (range:new-in dad pres)
  (present pres stream)
  (stream-flush stream)) 
;;
;;
(defstruct tag-promise tag start end)

(defun tag-in (stream tag)
  (make-tag-promise :tag tag :start (+ (offset stream) (index stream))))

(defun tag-out (stream promise)
  (setf (tag-promise-end promise) (+ (offset stream) (index stream)))
  (push promise (promises stream)))

(defmacro with-tag (stream tag &body body)
  (let ((strm (gensym))
	(promise (gensym)))  
    `(let* ((,strm ,stream)
	   (,promise (tag-in ,strm ,tag)))
       ,@body
       (tag-out ,strm ,promise))))

;; start and end are dad-based coordinates...
;; once we start a range-promise, we count from 0!
(defstruct range-promise range end)



(defun range-in (stream newrange)
  (with-slots (range offset index rangebase) stream
    (setf (range:dad newrange) range
	  range newrange)
    (values   
     (shiftf rangebase (+ offset index))
     (make-range-promise :range newrange))))

(defun range-out (stream oldbase promise)
  (with-slots (range offset index promises rangebase) stream
    (let ((here (+ offset index)))
      (setf (range:width range) (- here rangebase)
	    (range-promise-end promise) (- here oldbase)
	    range (range:dad range)
	    rangebase oldbase)
      (push promise promises))) )

(defmacro with-range (stream newrange &body body)
  ;; `(let ((it ,range)) (progn ,@body))
  (let ((oldbase (gensym))
	(promise (gensym))
	(strm (gensym)))
    `(let ((,strm ,stream)
	   (it ,newrange))
       (mvb (,oldbase ,promise) (range-in ,strm it)
	    ,@body
	    (range-out ,strm ,oldbase ,promise)))))


(defun stream-range-promises (stream)
  (loop for promise in (reverse (promises stream)) do
       (typecase promise	 
	 (range-promise
	  (with-slots (range end) promise
	    (range:sub range end)))
	 (tag-promise
	  (with-slots (start end tag) promise
	    (with-slots (iter iter1) stream
	      (%gtb-get-iter-at-offset stream iter start)
	      (%gtb-get-iter-at-offset stream iter1 end)
	      (gtb-apply-tag stream tag iter iter1 ))))))
  (setf (promises stream) nil))
