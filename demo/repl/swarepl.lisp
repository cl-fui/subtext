(in-package :subtext)



;;OK (ql:quickload :stext)(in-package :stext)
;;;=============================================================================
;;; Swank REPL buffer
;;;
(defclass swarepl (rbuffer)
  ((swank :initform nil :accessor swank) ;swank communication channel
   (sldbs :accessor sldbs :initform (make-hash-table)) ; track debuggers by '
   (read-id :accessor read-id :initform 0) ;0=commandline, otherwise read-line
   (read-tag :accessor read-tag :initform 0) ;TODO: terminology?
   )
  (:metaclass gobject-class))

;;==============================================================================
;; Define presentation types
(defpres p-entry (pres) ())
(defpres p-pres (pres)
  ((id :accessor id :initarg :id)))
(defpres p-input (pres) ())

(defmethod initialize-instance :after ((pbuf swarepl) &key (port 5000))
  (print "initialize-instance: swarepl")
  (setf *pbuf* pbuf);***
  ;;---------------------------------------------------------------------------
  ;; Associate presentations to this buffer
  (pres-tag pbuf p-entry (:foreground "AntiqueWhite" :editable nil) )
  (pres-tag pbuf p-pres  (:foreground "red" :editable nil));
  (pres-tag pbuf p-input (:foreground "blue" :editable t))

  ;;---------------------------------------------------------------------------

  
  (with-slots (swank) pbuf
    (setf swank (swa:make-connection "localhost" port))
    pbuf))

(defmethod -on-announce-eli :after ((pbuf swarepl) eli)
  (print "on-announce-eli")
  (eli-def
   eli (kbd "Return")
   (lambda ()
     (with-slots (swank read-id read-tag) pbuf
       (let ((string (simple-input-get-text pbuf)))
	 (if (zerop read-id)
	     (let ((line (swarepl-parse-string string)))
	       (when line
		 ;; Convert entered text to 'entry presentation
		 (simple-input-promise pbuf (make-instance 'p-entry ))
		 (swa:eval swank line #'prompt-proc)))
	     (progn; swank needs a newline!  A little ugly, but...
	       (swa:emacs-return-string
		swank (concatenate 'string string '(#\newline))
		read-id read-tag  )))))
     nil))
  (eli-def eli (kbd "C-x C-y") (lambda () (format t "OK!!!!!~&") nil)))
;;------------------------------------------------------------------------------

;; view invokes this on destruction...
(defmethod -on-destroy :before ((pbuf swarepl))
  (swa:disconnect (swank pbuf)))

(defmethod -on-initial-display :after ((pbuf swarepl))
  ;; all instance of p'entry share the tag 'input
  (format t "ON-INITIAL-DISPLAY of SWAREPL: ~A ~A~&" *standard-output* *package*)
    (with-slots (swank ) pbuf
      (swa:connect swank :fallback #'our-fallback :out *standard-output* :pack *package*)
      (swa:emacs-rex swank "(swank:connection-info)")
      (swa:emacs-rex swank "(swank:swank-require '(swank-presentations swank-repl))")
      (swa:emacs-rex swank  "(swank:init-presentations)")
      ;; This one replies with package and prompt...
      (swa:emacs-rex swank "(swank-repl:create-repl nil :coding-system \"utf-8-unix\")"
		     :proc (lambda (conn reply id);REX-CALLBACK
			     (declare (ignore id))
			     (setf (swa:pkg    conn) (first (second reply));symbol..
				   (swa:prompt conn)   (second (second reply))) ))
    
    ;;---------------------------------------------
    ;; This can be called explicitly
      (defun prompt (swank)
	(with-slots (read-id read-tag) pbuf
	  (setf read-id 0
	      read-tag 0))
	
	(with-tag ("prompt" pbuf)
	  (fresh-line pbuf)
	  (format pbuf "~A> " (swa:prompt swank)))
	(simple-input-mark pbuf))
      ;;----------------------------------------------
      ;; Callback for any eval issued, called on reply
      ;; careful! callbacks are from another thread!
      ;;
      (defun prompt-proc (swank reply id);REX-CALLBACK
	(declare (ignore id))
	;;(format t "~%PROMPT-PROC: ~A~&" reply)
	(if (eq :abort (first reply))
	    (gsafe (format pbuf (second reply))))
	(gsafe (prompt swank)))
      ;;---------------------------------------------------------------------
      ;; TODO: thread-safety?
      (defun sw-write-string (connection string &optional stream)
	(princ string pbuf))
      
      (let (pr)
	(defun sw-presentation-start (connection id stream)
	  (setf pr (tag-in pbuf (make-instance 'p-pres :id id))))
	
	(defun sw-presentation-end (connection id stream)
	  (tag-out pbuf pr)))
      
      (defun sw-new-package (connection name nickname)
	(setf (swa:pkg connection) name
	    (swa:prompt connection) nickname))
      
      ;;-----------------------------------------------------------------------
      ;; Input requested (read-line?).  Keep the id and tag in a range to return
      ;; later, when <enter> is processed.
      (defun sw-read-string (connection id tag)
	(with-slots (read-id read-tag) pbuf
	  (setf read-id id
		read-tag tag))
	(simple-input-mark pbuf))
      
      
      ;; We shall keep the debuggers around in a hashtable, keyed by both thread
      ;; and debug level (TODO: is this really necessary?).
      ;;
      ;; First, debug is invoked with all kinds of useful data.  We will use this
      ;; occasion to prepare an sldb (buffer) and a view stored right in sldb.
      (defun sw-debug (connection thread level condition restarts frames conts)
	;;(format t "SW-DEBUG: conn ~A thread ~A level ~A&" connection thread level)
	(setf (gethash (+ level (* 1000 thread)) (sldbs pbuf))
	      (make-vsldb connection thread level condition restarts frames conts)))
      ;; Then, this guy comes to activate the debugger.  We will create a window
      ;; and a frame for it, and display it.
      (defun sw-debug-activate (connection thread level &optional selectl)
	(let ((vsldb (gethash (+ level (* 1000 thread)) (sldbs pbuf))))
	  (when vsldb
	    (make-framed-view vsldb
			      :title (format nil "SLDB ~A ~A" thread level)
			      :kill nil ) 
	    (vsldb-activate vsldb))))
      ;; Now when this returns, we destroy the widget and remove SLDB from the
      ;; hashtable.  TODO: are all sub-widgets destroyes?
      (defun sw-debug-return (connection thread level stepping)
	;;(format t "sw-dbug-return: ~A ~A ~A~&" thread level stepping)
	(let ((vsldb (gethash (+ level (* 1000 thread)) (sldbs pbuf))))
	  (when vsldb
	    (rview-destroy-top vsldb)
	    (remhash (+ level (* 1000 thread)) (sldbs pbuf)))))
      
      ;; Start ball-roll
      (gsafe (prompt swank))))

 
;;------------------------------------------------------------------------------


;; Before evaluating a string via SWANK, we read it here, discarding sexps
;; just to make sure it is syntactically OK.  TODO: is this good enough?
;; We rely on the behavior of read-from-string nil +eof+.  An actual EOF
;; condition here means we have a malformed sexp...
;; For consistency, we right-trim ws off (helps with history)
;;
(defparameter ws-bag (format nil " ~C~C" #\tab #\newline))
(defun swarepl-parse-string (string)
  "parse string, discarding sexps. When well-formed, return string after
 right-trimming ws"
  (mvb (result error) 
       (ignore-errors; on error (to wit EOF), malformed sexp... 
	 (let ((+eof+ (gensym)))
	   (loop named forms with start = 0 do ;exit on error or eof
		(mvb (result at)
		     (with-standard-io-syntax
		       (read-from-string string nil +eof+ :start start))
		     (when (eq result +eof+); eof here means done between forms
		       (return-from forms (string-right-trim ws-bag string)))
		     (setf start at)))))
       (declare (ignorable error))
       result))


 

;;
;;	    
;;
;;(let ((sexp 
;;(case (car sexp) (:write-string `(swax:write-string ,(cdr sexp)))    (t (print message)))
;;(swa:request-listener-eval *swank* "2")
;; Fallback routine registered with swank (see init-swank!).  Handles all but
;; return messages (which are handled with their respective lambdas.
;; 
(defun our-fallback (connection message)
  "Process a message from swank. return result or nil."
  (let ((fun (case (first message)
	       (:write-string       #'sw-write-string)
	       (:presentation-start #'sw-presentation-start)
	       (:presentation-end   #'sw-presentation-end)
	       (:new-package        #'sw-new-package)
	       (:read-string        #'sw-read-string)
	       (:debug              #'sw-debug)
	       (:debug-activate     #'sw-debug-activate)
	       (:debug-return       #'sw-debug-return)
	       (t (format t "~%FALLBACK: UNKNOWN FORM:~%~A~&" message)
		  nil))))
    (and fun (gsafe (apply fun connection (rest message))))))


(defun pbuf-yank (pbuf)
  ;;(print "pasting")
  
  (gtb-paste-clipboard pbuf (gtk-clipboard-get "PRIMARY")
		       :default-editable t)
  )

;;======================================================================
;; history for repl
;;
(defun pbuf-hist-prim (pbuf replacement)
 #|| (when replacement; could be nill on startup...
    (with-slots ( histid) pbuf
      (let ((range (range:at (root pbuf) (gtb-cursor-position pbuf))))
	(when (eq 'p-entry (type-of range))
	  ;; we only work on p-entry ranges
	  (mvb (start end) (range-iters pbuf range)
	       ;;(print start) (print end)
	       (unless (gti-equal start end)
		 (gtb-delete pbuf start end))
	       (gtb-insert pbuf replacement :position start))))))
||#)

(defun pbuf-c-up (pbuf)
  (pbuf-hist-prim pbuf (swa:history-back (swank pbuf))))

(defun pbuf-c-down (pbuf)
  (pbuf-hist-prim pbuf (swa:history-forward (swank pbuf))))





;;==============
(defun repl( &key (stdout *standard-output*) (package *package*))
  "final"
  ;;(format t "AAA STANDARD OUTPUT?~A ~A ~&"*standard-output* *package*)
  (within-main-loop
    (setf *standard-output* stdout); Why can't I just bind it?
    (let* ((*standard-output* stdout)
	   (*package* package)			;re-enable output
;;	   (ass  (format t "STANDARD OUTPUT?~A ~A ~&"*standard-output* *package*))
	   (top (make-frame (make-window (make-rview (make-instance 'swarepl))
					 :ml  "-:**- *slime repl*") 
			    :title "Subtext REPL"
			    :kill t)))
     
      (princ "Take this REPL, brother! ..." *echo*)
      (gtk-widget-show-all top)
      (-on-initial-display top))))
