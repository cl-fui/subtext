(in-package :stext)


(defstruct (p-entry  (:include range:range)) )
(defstruct (p-pres   (:include range:range)) id )
(defstruct (p-input  (:include range:range)) id tag )
(defstruct (p-prompt (:include range:range)) )


;;OK (ql:quickload :stext)(in-package :stext)
(defclass swarepl (rbuffer)
  ((swank :initform nil :accessor swank) ;swank communication channel
   (sldbs :accessor sldbs :initform (make-hash-table)); track debuggers by '
   )
  (:metaclass gobject-class))

(defun make-swarepl ()
  (make-instance 'rview
		 :buffer (setf *pbuf* (make-instance 'swarepl))))



;; view invokes this on destruction...
(defmethod -on-destroy ((pbuf swarepl))
  (swa:disconnect (swank pbuf))
  )






(defun init-swank (pbuf)
  (with-slots (swank ) pbuf
    (swa:connect swank #'our-fallback)
    (swa:emacs-rex swank "(swank:connection-info)")
    (swa:emacs-rex swank "(swank:swank-require '(swank-presentations swank-repl))")
    (swa:emacs-rex swank  "(swank:init-presentations)")
    ;; This one replies with package and prompt...
    (swa:emacs-rex swank "(swank-repl:create-repl nil :coding-system \"utf-8-unix\")"
		   :proc (lambda (conn reply id);REX-CALLBACK
		     (declare (ignore id))
		     (setf (swa:pkg conn) (first (second reply));symbol..
			   (swa:prompt conn)  (second (second reply))) ))
 
    ;;feed the engine
    (defun prompt (swank)
      (stream-delimit pbuf (make-p-prompt))
      (with-tag pbuf "prompt"
	(fresh-line pbuf)
	(format pbuf "~A> " (swa:prompt swank)))
      (stream-delimit pbuf (make-p-entry) ))
        
    ;;---------------------------------------------
    ;; callback evaluated upon processing of a command line
    (defun prompt-proc (swank reply id);REX-CALLBACK
      (declare (ignore id))
      ;;(format t "~%PROMPT-PROC: ~A~&" reply)
      (if (eq :abort (first reply))
	  (gsafe (format  pbuf (second reply))))
      (gsafe (prompt swank)))
    (defun sw-write-string (connection string &optional stream)
      (princ string pbuf))

    (defun sw-presentation-start (connection id stream)
      (stream-delimit pbuf (make-p-pres :id id) ) ;for now,just delimit with presentation id
      )
    
    (defun sw-presentation-end (connection id stream)
      (finish-output pbuf)
      (pbuf-tag-range pbuf  "pres")
      (stream-delimit pbuf nil ) ;for now,just delimit with presentation id
      )
    
    (defun sw-new-package (connection name nickname)
      (setf (swa:pkg connection) name
	    (swa:prompt connection) nickname))
    
    ;;-----------------------------------------------------------------------
    ;; Input requested (read-line?).  Keep the id and tag in a range to return
    ;; later, when <enter> is processed.
    (defun sw-read-string (connection id tag)
      (stream-delimit pbuf (make-p-input :id id :tag tag))
      )

    ;; We shall keep the debuggers around in a hashtable, keyed by both thread
    ;; and debug level (TODO: is this really necessary?).
    ;;
    ;; First, debug is invoked with all kinds of useful data.  We will use this
    ;; occasion to prepare an sldb (buffer) and a view stored right in sldb.
    (defun sw-debug (connection thread level condition restarts frames conts)
      ;;(format t "SW-DEBUG: conn ~A thread ~A level ~A&" connection thread level)
      (setf (gethash (+ level (* 1000 thread)) (sldbs pbuf))
	    (make-wsldb connection thread level condition restarts frames conts)))
    ;; Then, this guy comes to activate the debugger.  We will create a window
    ;; and a frame for it, and display it.
    (defun sw-debug-activate (connection thread level &optional selectl)
      (let ((wsldb (gethash (+ level (* 1000 thread)) (sldbs pbuf))))
	(when wsldb
	  (let ((frame
		 (make-frame
		  (make-window  wsldb)
		  :title (format nil "SLDB ~A ~A"thread level)
		  :kill nil)))
	    ;(setf (sldb-fr sldb) frame)
	    (gtk-widget-show-all frame))
	  (wsldb-activate wsldb))))
    ;; Now when this returns, we destroy the widget and remove SLDB from the
    ;; hashtable.  TODO: are all sub-widgets destroyes?
    (defun sw-debug-return (connection thread level stepping)
      ;;(format t "sw-dbug-return: ~A ~A ~A~&" thread level stepping)
      (let ((wsldb (gethash (+ level (* 1000 thread)) (sldbs pbuf))))
	(when wsldb
	  (wsldb-destroy wsldb)
	  (remhash (+ level (* 1000 thread)) (sldbs pbuf))
	  (PRINT "SW-DEBUG-RETURN DONE"))))
    ;; Start ball-roll
    (prompt swank)))

 
;;------------------------------------------------------------------------------
(defmethod initialize-instance :after ((pbuf swarepl) &key)
  (print "initialize-instance: swarepl")
  (setf *pbuf* pbuf)
;;  (format *pbuf* "fuck")
;;  (funcall (flush *pbuf*))
  (with-slots (swank) pbuf
    (setf swank (swa:make-connection "localhost" 5000))
    (init-swank pbuf))
  ;; install <enter> key binding
  
  
  pbuf)

(defmethod -on-announce-eli :after((pbuf swarepl) eli)
  (with-slots (keymap) eli
    (labels
	((pbuf-idle-entry () ;in-scope for pbuf!
	   (with-slots (swank) pbuf
	     (let* ((range (range:at (root pbuf) (gtb-get-char-count pbuf)))
		    (string (range-text pbuf range))
		    )
	       (typecase range
		 (p-input ;;(:emacs-return-string 1 5 "88\n")
		  (with-slots (id tag) range
		    (swa:emacs-return-string swank string id tag)))
		 (p-entry
		  (swa:eval (swank pbuf) ;try to parse string, may be null
			    (pbuf-parse-string string) #'prompt-proc)))))
	   nil)); remove thyself
      (keymap-bind
       keymap "<RET>"
       (lambda ()
	 (let ((iter (gtb-get-iter-at-mark pbuf (gtb-get-insert pbuf))))
	   ;; only allow input at the end!
	   (if (gti-is-end iter)
	       (gdk-threads-add-idle #'pbuf-idle-entry)))
	 nil; let gtk insert the <enter> into the buffer...
	 )))
    
    )
  )
;;------------------------------------------------------------------------------

;; Before evaluating a string via SWANK, we read it here, discarding sexps
;; just to make sure it is syntactically OK.  TODO: is this good enough?
;; We rely on the behavior of read-from-string nil +eof+.  An actual EOF
;; condition here means we have a malformed sexp...
;; For consistency, we right-trim ws off (helps with history)
;;
(defparameter ws-bag (format nil " ~C~C" #\tab #\newline))
(defun pbuf-parse-string (string)
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
;;==============================================================================
;; Prompt
;; called from init, then, when swank return is processed by callback prompt-proc.


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
  (when replacement; could be nill on startup...
    (with-slots ( histid) pbuf
      (let ((range (range:at (root pbuf) (gtb-cursor-position pbuf))))
	(when (eq 'p-entry (type-of range))
	  ;; we only work on p-entry ranges
	  (mvb (start end) (range-iters pbuf range)
	       ;;(print start) (print end)
	       (unless (gti-equal start end)
		 (gtb-delete pbuf start end))
	       (gtb-insert pbuf replacement :position start)))))))

(defun pbuf-c-up (pbuf)
  (pbuf-hist-prim pbuf (swa:history-back (swank pbuf))))

(defun pbuf-c-down (pbuf)
  (pbuf-hist-prim pbuf (swa:history-forward (swank pbuf))))

(defmethod prez-on-button-press ((prez null) pbuf range button)
  (print "prez null")
  nil)
(defmethod prez-on-button-press ((prez t) pbuf range button)
  (print "prez t")
  nil)


(defmethod prez-on-button-press ((it p-prompt) pbuf range button)
  (if (= button 1)
      nil ;default handler
      (progn
	(popup-menu '(("fuck") ("duck")) :button button)
	(print "prez pressed promjpt")
	t; we handle
	)))




