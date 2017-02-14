;; 
;; TB - base textbuffer
;;
;; We start with a gtk-text-buffer.  We add a position-tracking system for
;; pmarks. All inserts and deletes into the buffer cause transparent updates
;;
;; Anchor is an offset into the buffer for streams, managed.
;;
(in-package :subtext)

;;;=============================================================================
;;; tb - An augmented gtk text buffer with a position-tracking system.
;;;
(defclass tb (gtk-text-buffer)
  ((iter   :accessor iter   :initform nil :type gtk-text-iter)
   (iter1  :accessor iter1  :initform nil :type gtk-text-iter)
   ;(root   :accessor root   :initform nil)
   (anchor :accessor anchor :initform 0 :type fixnum)
   ;; on-insert-text-before keeps insert start position prior to expansion
   (oldx   :accessor oldx   :initform 0   :type fixnum) ;insert-text position

   (old-mouse :accessor old-mouse :initform nil );old mouse presentation-lists
   ;; promises subsystem... Really requires a stream 
   (promises     :accessor promises :initform nil)
   )
  
  (:metaclass gobject-class))

;;------------------------------------------------------------------------------
(defmethod initialize-instance :after ((buffer tb) &key )
  (print "initialize-instance: tb")
  (with-slots (iter iter1 root anchor) buffer
    (setf iter   (gtb-get-start-iter buffer)
	  iter1  (gtb-get-end-iter   buffer)
	 ; root (make-instance 'pres )
  ))
  (g-signal-connect buffer "insert-text" #'on-insert-text-before :after nil)
  (g-signal-connect buffer "insert-text" #'on-insert-text-after :after t)
  (g-signal-connect buffer "delete-range" #'on-delete-range)
  (setf)
  )
;;==============================================================================
;; A major task is maintaining the position table on inserts and deletes!
;;
;; We hook 'on-insert-text' handlers, but these report len in bytes
;; and we need utf8 characters!  So we hook both before and after and figure
;; out the length from iters.
;;
;; keep track of insert position prior to insertion
(defun on-insert-text-before (buffer iter text len)
  (declare (ignore text len)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (setf (oldx buffer) (the fixnum (gti-offset iter))))

;;
(defun on-insert-text-after (buffer iter text len)
  "update presentation bounds"
  (declare (ignore text len)
	   (optimize (speed 2) (safety 0) (debug 0)))
  (let* ((offset (oldx buffer))
	 (chars (- (the fixnum (gti-offset iter)) offset)))
    (declare (type fixnum offset chars))
;;      (format t "on-insert-text-at ~A characters at ~A~&" chars offset )
    ;; maintain a right-gravity anchor 
    (when (<= offset (the fixnum (anchor buffer)))
      (incf (the fixnum (anchor buffer))  chars))
;;    (mtree:widen  (mtree:at (mtree buffer) (oldx buffer))     chars)
))

;; TODO: fix this!
(defun on-delete-range (buffer istart iend)
  ;;(print "AFLJALSDJ~&")
  (let ((o1 (gti-get-offset  istart))
	(o2 (gti-get-offset  iend)))

    (when (<= o1 (the fixnum (anchor buffer)))
      (decf (the fixnum (anchor buffer)) (- o2 o1))
      (format t "~%deleting range: [~D ~D)~&" o1 o2))
   ;; (range:narrow (range:at (root  buffer) o1) (- o2 o1))
))

;;==============================================================================;
;; a quick way to make a tag
(defmacro pbuf-new-tag (pbuf &rest x)
  `(let ((tag (make-instance 'gtk-text-tag ,@x)))
     (gttt-add (gtb-tag-table ,pbuf) tag)
     tag))


;;===============================================================================
;; Motion handler
;;
(defgeneric -pres-on-mouse (pres inp))

(defmethod -pres-on-mouse ((pres t) inp))

;; TODO: this loses order!

(defun presentations-on-motion (tb old new)
  "scan the old and new lists of presentations, and note presentations that are
different.  For all presentations that are no longer in the list, call exiting;
for all newly introduced ones, call entering.  Return new."
  (let* ((same (intersection old new)); these have not changed...
	 (out (set-difference old same)); these are phased out.
	 (in  (set-difference new same))) ; and these are newly introduced.
    (format t "ON_MOTION SAME: ||~A||~&  IN:~A OUT: ~A~&" same in out)
    (loop for pres in out do (-pres-on-mouse pres nil))
    (loop for pres in in  do (-pres-on-mouse pres t))
    new
))

(defmethod -on-motion ((tb tb) iter event)
  (with-slots (old-mouse) tb
    (setf old-mouse
	  (presentations-on-motion tb old-mouse (presentations-at tb iter)))))

;;===============================================================================
;; Mouse click handler
;;
;; presentation handlers return T if done, or NIL to propagate click.
(defgeneric -pres-on-button (pres  button))
(defmethod  -pres-on-button ((p t) button) nil)

(defmethod -on-button ((tb tb) iter event)
 ;; (print "ONBUG")
  (let ((presentations (presentations-at tb iter))
	(times (gdk-event-get-click-count event))
	(button (gdk-event-button-button event)))
    (loop for pres in presentations
       until (-pres-on-button pres button))))


(defmethod -on-announce-eli ((pbuf tb) eli)  )
