(in-package :subtext)

(defcontext pnormal (ctx) ())
(defcontext psexp (ctx) (payload) :keymap t)

;;OK (ql:quickload :stext)(in-package :stext)
;;;=============================================================================
;;; Swank REPL buffer
;;;
(defclass buflisp (mark-out-stream)
  ((parens  :accessor parens  :initform nil)
   (pindex  :accessor pindex  :initform 0))
  
  (:metaclass gobject-class))

(print (find-class 'buflisp))

;;==============================================================================
;; Define presentation types.  ps are for parenthesized sexps
(defcontext ps00 (psexp) (payload))
(defcontext ps01 (psexp) (payload))
(defcontext ps02 (psexp) (payload))
(defcontext ps03 (psexp) (payload))
(defcontext ps04 (psexp) (payload))
(defcontext ps05 (psexp) (payload))
(defcontext ps06 (psexp) (payload))
(defcontext ps07 (psexp) (payload))
(defcontext ps08 (psexp) (payload))
(defcontext ps09 (psexp) (payload))

(defcontext p-atom (ctx) (payload))
(defcontext p-unk  (ctx) (payload))

(defmethod initialize-instance :after ((pbuf buflisp) &key )
  (print "initialize-instance: buflisp")
  (setf *pbuf* pbuf);***
  ;; for now keep array of paren symbols.
  (setf (parens pbuf)
	(make-array 10 :initial-contents
		    '(ps00 ps01 ps02 ps03 ps04 ps05 ps06 ps07 ps08 ps09)))
	;;---------------------------------------------------------------------------
	;; Associate presentations to this buffer.  Priority is important!
  (pbuf-new-tag pbuf :name "normal"  :foreground "AntiqueWhite1" :editable t)
  
  (context-tag pbuf ps00  (:foreground "#FFCC33" :editable t) )
  (context-tag pbuf ps01  (:foreground "#CCFF33" :editable t) )
  (context-tag pbuf ps02  (:foreground "#33FF66" :editable t) )
  (context-tag pbuf ps03  (:foreground "#FF6633" :editable t) )
  (context-tag pbuf ps04  (:foreground "#33FFCC" :editable t) )
  (context-tag pbuf ps05  (:foreground "#FF3366" :editable t) )
  (context-tag pbuf ps06  (:foreground "#33CCFF" :editable t) )
  (context-tag pbuf ps07  (:foreground "#FF33CC" :editable t) )
  (context-tag pbuf ps08  (:foreground "#CC33FF" :editable t) )
  (context-tag pbuf ps09  (:foreground "#3366FF" :editable t) )
  
  (context-tag pbuf p-unk   (:foreground "yellow" :editable t) (:left-gravity t))
  (context-tag pbuf p-atom  (:foreground "green" :editable t) )
  ;;----------------------------------------------------------------------------
  (clrf keymap-psexp)
  (keymap-def keymap-psexp (kbd "C-x C-c") (lambda () (format t "YESSSSSS!!!!~&")) )
  
  (g-signal-connect
     pbuf "notify::cursor-position"
     (lambda (gobject gparamspec)
       (terpri *standard-output*)
     ;  (format t "NOTIFY:CURPOS Setting pos at ~A~&" (gtb-cursor-position pbuf))
       (with-slots (mark iter) pbuf
	 (pbuf-iter-to-cursor pbuf)
	 (gtb-move-mark pbuf mark iter)
	;; (bufstat pbuf)
	 ))) 
  (prin pbuf (pr 'p-unk () "  "))
)


(defmethod -on-announce-eli :after ((pbuf buflisp) eli)
  (print "on-announce-eli")
  (with-slots (pindex parens) pbuf
    (eli-def
     eli (kbd "(")
     (lambda ()
					; (format t ">..~A ~A~&" parens pindex)
   
       (prin pbuf (pr (aref parens pindex) ()  "()"))
       (incf pindex)

       t)))
   (eli-def
    eli (kbd "F1")
    (lambda ()
      (bufstat pbuf)))
   
  (eli-def
   eli (kbd "Return")
   (lambda ()
     nil)))
;;------------------------------------------------------------------------------

    ;(promises-free stream promises)
    
(defun t4 ( &key (stdout *standard-output*) (package *package*))
  "final"
  ;;(format t "AAA STANDARD OUTPUT?~A ~A ~&"*standard-output* *package*)
  (within-main-loop
    (setf *standard-output* stdout); Why can't I just bind it?
    (let* ((*standard-output* stdout)
	   (*package* package)			;re-enable output
	   (buffer(make-instance 'buflisp))
	   (top (make-frame (make-window (make-rview buffer)) 
			    :kill t)))
     
      (gtk-widget-show-all top)
      (-on-initial-display top)
      
      (format buffer "SHOWING~&")
      (prin buffer "hello " (pr 'ps00 () "p0..." (pr 'ps01 () "p1...") "p0 again"
				(pr 'ps02 () ".2.." (pr 'ps03 () "3/")) "and 0") "---")
))
  )
()
