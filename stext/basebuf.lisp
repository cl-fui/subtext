(in-package :stext)
;;;=============================================================================
;;; basebuf - some basic buffer functionality
;;;
;;OK (ql:quickload :stext)(in-package :stext)
(defclass basebuf (tb) 
  ((ptags :accessor ptags     :initform nil )) 
  (:metaclass gobject-class))

;; For now, differentiate our presentation tags
(defclass ptag (gtk-text-tag)
  ()
  (:metaclass gobject-class))


;;------------------------------------------------------------------------------

(defmethod initialize-instance :after ((buffer basebuf) &key )
   (print "initialize-instance: basebuf")
  (with-slots (ptags range root) buffer
    ;; establish buffer modification handlers to sync with the range system
    (pbuf-create-tags buffer); for now...TODO
    ); for noww...TODOon-in
  ;;(print "initialize-instance: basebuf DONE")
  )

(defparameter *self-inserting-keys* " 0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*()_+[]{}\|~`,.<>/?")
(defmethod -on-eli-key ((pbuf basebuf) key event)
  "process an eli key"
  #||
  (let ((char (key->character key)))
    (if (find char *self-inserting-keys* :test #'char=)
	(progn (write-char char pbuf) (finish-output pbuf) t)
	nil))
||# nil)


(defmethod -on-announce-eli  ((pbuf basebuf) eli)
  (with-slots (keymap) eli
;;    (keymap-define-key    keymap #.kb:BS (lambda (gtkkey) (declare ) (pbuf-key-backspace pbuf)) )
    (keymap-define-key
     keymap #.kb:F1
     (lambda (gtkkey) (declare (ignore gtkkey))
	     (let ((iter (gtb-get-iter-at-mark pbuf (gtb-get-insert pbuf))))
	       (bufstat-prim pbuf (gti-get-offset iter)))
	     t))))

(defmethod -on-destroy :before ((buffer basebuf))
  (format t "BASEBUF ON-DESTROY ~A~&" buffer ))


(defmethod clear ((pbuf basebuf))
  "clear pbuf of all presentations, and all content!"
  (pbuf-bounds pbuf)
  (with-slots (iter iter1) pbuf
    (gtb-remove-all-tags pbuf iter iter1)
    (gtk::%gtk-text-buffer-delete pbuf iter iter1)))



;;==============================================================================
;; Debugging tools
;;
(defun bufstat-prim (pbuf offset)
  (with-slots (iter iter1) pbuf
    (let*
	((here (gtb-get-iter-at-offset pbuf offset))
	 (marks-here (gti-get-marks here))
	 (tags-here (gti-get-tags here)))
      ;; are we inside a presentation?
     ;
     
	   
	   ;; (setf *q* range)g

      (format t "~%===============================================")
      (format t "~%Cursor is at ~D; character [~C](~d $~x)" offset
	      (gti-get-char here) (char-code (gti-get-char here))
	      (char-code (gti-get-char here)))
	   
      (format t "~%Tags here")
      (loop for tag in tags-here do (print tag))
	   ;;(format t "~%Tags here: ~A" tags-here)

      (format t "~%Marks here: ~A" marks-here )
;;      (if pres	  (format t "~%presentations ~A ~A at ~A ~A~&" pres off iter iter1   ))
      
      ;;      (format t "~%Inside presentation: ~A" (pbuf-get-pres pbuf here))
      )))

;; 
(defun bufstat (buffer) ;;see gtk-ui for keybinding
 
  (bufstat-prim buffer (gtb-cursor-position buffer)) )

;;==============================================================================;
;; a quick way to make a tag
(defmacro pbuf-new-tag (pbuf &rest x)
  `(let ((tag (make-instance 'ptag ,@x)))
     (gttt-add (gtb-tag-table ,pbuf) tag)
     tag))



(defun pbuf-create-tags (pbuf)
   
  (with-slots (ptags) pbuf
    (setf
     ptags (make-array 10)
     (aref ptags 5) (pbuf-new-tag pbuf :name "error"  :foreground "blue" :editable t;nil
				  )
     

     (aref ptags 0) (pbuf-new-tag pbuf :name "prompt" :foreground "cyan" :editable t;nil
				  )
     (aref ptags 4) (pbuf-new-tag pbuf :name "pres"   :foreground "red" :editable t;nil
				  )
     (aref ptags 1) (pbuf-new-tag pbuf :name "input"  :foreground "AntiqueWhite1" :editable t)
     (aref ptags 2) (pbuf-new-tag pbuf :name "output" :foreground "NavajoWhite2":editable t;nil
				  )
     (aref ptags 3) (pbuf-new-tag pbuf :name "return" :foreground "chartreuse1" :editable t;nil
				  )
  
     (aref ptags 6) (pbuf-new-tag pbuf :editable t;nil
				  )
     (aref ptags 7) (pbuf-new-tag pbuf :foreground "deepskyblue")
     (aref ptags 8) (pbuf-new-tag pbuf :name "bg-greenish" :editable t;nil
				  :foreground "deepskyblue";
				  :background-rgba  (make-gdk-rgba :green 1.0d0 :alpha 0.1d0) )
     (aref ptags 9) (pbuf-new-tag pbuf :editable t;nil
				  :background-rgba (make-gdk-rgba :blue 1.0d0 :alpha 0.1d0)
				  :foreground "deepskyblue";
				  )
     
     )))

;; Move cursor to end and create a subrange at end...


 
(defun pbuf-tag-at-offsets (stream tag start end)
  (with-slots (iter iter1) stream
    (pbuf-set-iters stream start end)
    (gtb-apply-tag stream tag iter iter1))
)
#||
(defun pbuf-range-iters (buffer range)
  (with-slots (iter iter1) buffer
    (mvb (start end) (range:bounds range)
	 (%gtb-get-iter-at-offset buffer iter start)
	 (%gtb-get-iter-at-offset buffer iter1 end))))


(defun pbuf-range-text (buffer range)
  (with-slots (iter iter1) buffer
    (pbuf-range-iters buffer range)
    (gtb-get-text buffer iter iter1 nil)))
||#

(declaim (inline pbuf-set-iters))
(defun pbuf-set-iters (buffer off off1)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (iter iter1) buffer
    (%gtb-get-iter-at-offset buffer iter  off )
    (%gtb-get-iter-at-offset buffer iter1 off1 )))

(defun pbuf-bounds (buffer)
  (with-slots (iter iter1) buffer
    (%gtb-get-start-iter buffer iter )
    (%gtb-get-end-iter   buffer iter1 )))

#||
(defun pbuf-range-minimize (buffer range)
  "minimize a range to a single character at the end"
  (with-slots (iter iter1) buffer
    (pbuf-range-iters buffer range)
    (setf (range:child range) nil) ;eliminate children
    (gtb-remove-all-tags buffer iter iter1)
    (gti-backward-char iter1)
    (%gtb-delete buffer iter iter1)))
||#
