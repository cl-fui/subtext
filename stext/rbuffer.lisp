(in-package :stext)
;;;=============================================================================
;;; rbuffer - a gtk-text-buffer that is also a range root.
;;;
;;OK (ql:quickload :stext)(in-package :stext)
(defclass rbuffer (gtbstream ) 
  ((ptags :accessor ptags     :initform nil )
   (root  :accessor root      :initform (range:make) ))
  (:metaclass gobject-class))

;;------------------------------------------------------------------------------

(defmethod initialize-instance :after ((buffer rbuffer) &key )
   ;; (print "initialize-instance: rbuffer")
  (g-signal-connect buffer "insert-text" #'on-insert-text :after nil)
  (g-signal-connect buffer "delete-range" #'on-delete-range)
  (with-slots (ptags) buffer
    ;; establish buffer modification handlers to sync with the range system
   (pbuf-create-tags buffer); for now...TODO
   )
  ;;(print "initialize-instance: rbuffer DONE")
  )

(defmethod -on-announce-eli  ((pbuf rbuffer) eli)
    (with-slots (keymap) eli
    (keymap-bind
     keymap "<F1>"
     (lambda ()
       (let ((iter (gtb-get-iter-at-mark pbuf (gtb-get-insert pbuf))))
	 (bufstat-prim pbuf (gti-get-offset iter)))
       t))
    ))
(defmethod -on-destroy :before ((buffer rbuffer))
  (format t "RBUFFER ON-DESTROY ~A~&" buffer )
)

(defmethod clear ((pbuf rbuffer))
  "clear pbuf of all presentations, and all content!"
  (let ((start  (gtb-get-start-iter pbuf))
	(end    (gtb-get-end-iter   pbuf)))
    (gtb-remove-all-tags pbuf start end)
    (gtk::%gtk-text-buffer-delete pbuf start end)))

;; This is where the magic happens. We locate the range, and insert right into it...
(defun on-insert-text (buffer iter text len)
  "update presentation bounds"
  (declare (ignore text))
  (let ((off (gti-get-offset iter)))
    ;;(format t "~%WIDENING: at ~A by ~A~% "off len);;
    (range:widen (range:at (root buffer) off) len)))
  ;;    (format t  "~%Current range is ~A  ~A wide" (range:range-abs (range *out*))    (range:range-width (range *out*)))
  

(defun on-delete-range (buffer istart iend)
  "Delete marks in range.  Marks start to the left of a presentation."
  (let ((o1 (gti-get-offset  istart))
	(o2 (gti-get-offset  iend)))
;;    (format t "~%deleting range: [~D ~D)" o1 o2)
    (range:narrow (range:at (root  buffer) o1) (- o2 o1))))


;;==============================================================================
;; Debugging tools
;;
(defun bufstat-prim (pbuf offset)
  (let* (
	 (here (gtb-get-iter-at-offset pbuf offset))
	 (marks-here (gti-get-marks here))
	 (tags-here (gti-get-tags here)))
    (mvb (range off) (range:at (root pbuf) offset )
	;; (setf *q* range)g
	 (format t "~%===============================================")
	 (format t "~%Cursor is at ~D; character [~C](~d $~x)" offset
		 (gti-get-char here) (char-code (gti-get-char here))
		 (char-code (gti-get-char here)))

	 (format t "~%Tags here")
	 (loop for tag in tags-here do (print tag))
	 ;;(format t "~%Tags here: ~A" tags-here)
	 (format t "~%Marks here: ~A" marks-here )
	 (format t "~%presentations ~A ~A" range off   ))
    ;;      (format t "~%Inside presentation: ~A" (pbuf-get-pres pbuf here))
    ))

;; 
(defun bufstat (buffer) ;;see gtk-ui for keybinding
 
  (bufstat-prim buffer (gtb-cursor-position buffer)) )




;;==============================================================================;
;; a quick way to make a tag
(defmacro pbuf-new-tag (pbuf &rest x)
  `(let ((tag (make-instance 'gtk-text-tag ,@x)))
     (gttt-add (gtb-tag-table ,pbuf) tag)
     tag)
)

(defun pbuf-create-tags (pbuf)
   
  (with-slots (ptags) pbuf
    (setf
     ptags (make-array 10)
     (aref ptags 0) (pbuf-new-tag pbuf :name "prompt" :foreground "cyan" :editable nil)
     (aref ptags 1) (pbuf-new-tag pbuf :name "input"  :foreground "AntiqueWhite1" :editable t)
     (aref ptags 2) (pbuf-new-tag pbuf :name "output" :foreground "NavajoWhite2":editable nil)
     (aref ptags 3) (pbuf-new-tag pbuf :name "return" :foreground "chartreuse1" :editable nil)
     (aref ptags 4) (pbuf-new-tag pbuf :name "pres"   :foreground "red" :editable nil)
     (aref ptags 5) (pbuf-new-tag pbuf :name "error"  :foreground "blue" :editable nil)
     (aref ptags 6) (pbuf-new-tag pbuf :editable nil)
     (aref ptags 7) (pbuf-new-tag pbuf :foreground "deepskyblue")
     (aref ptags 8) (pbuf-new-tag pbuf :name "bg-greenish" :editable nil
				  :foreground "deepskyblue";
				  :background-rgba  (make-gdk-rgba :green 1.0d0 :alpha 0.1d0) )
     (aref ptags 9) (pbuf-new-tag pbuf :editable nil
				  :background-rgba (make-gdk-rgba :blue 1.0d0 :alpha 0.1d0)
				  :foreground "deepskyblue";
				  )
     
     )))

;; Move cursor to end and create a subrange at end...


 
 
(defun pbuf-tag-range (pbuf tag)
  (mvb (start end) (range:bounds (range:at (root  pbuf) (gtb-get-char-count pbuf)))
       (gtb-apply-tag pbuf tag
		      (gtb-get-iter-at-offset pbuf start)
		      (gtb-get-iter-at-offset pbuf end))))
;;---------------------------------------------
;; helpers



#||
(defun  (pbuf istart end)
  "get text"
  (gtb-get-text
   pbuf
   (gtb-get-iter-at-offset pbuf start )
   (gtb-get-iter-at-offset pbuf end )
   nil))

(defun pbuf-range-iters (pbuf range)
  "get the iters for a range"
  (mvb (start end) (range:bounds range)
       (values
	(gtb-get-iter-at-offset pbuf start)
	(gtb-get-iter-at-offset pbuf end))))

(defun pbuf-range-string-data (pbuf range)
  "values string data"
  (mvb (start end) (pbuf-range-iters pbuf range)
       (values
	(gtb-get-text pbuf start end nil)
	(range:data range))))
||#
(defun range-iters (buffer range)
  (mvb (start end) (range:bounds range)
       (values (gtb-get-iter-at-offset buffer start)
	       (gtb-get-iter-at-offset buffer (1- end)))))

(defun range-text (buffer range)
  (mvb (istart iend) (range-iters buffer range)
       (gtb-get-text buffer istart iend nil)))


;;of limited use, only good for top subranges inserted at end...
(defun stream-delimit (bufstrm range)
  (with-slots (flush ) bufstrm
    (funcall flush)
    (range:new-in (root bufstrm) range)))

(defun stream-anchor (bufstrm)
  (with-slots (flush) bufstrm
    (gtb-insert-child-anchor bufstrm (funcall flush))))











