(in-package :subtext)

;; Key-binding presentations - contain a class slot with
;; bindings.
(defpres kpres (pres)
  ((keymap :accessor keymap :initform nil))
  )

;; parent 
(defpres pnormal (pres) ())
(defpres psexp (kpres) (payload))

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
(defpres ps00 (psexp) (payload))
(defpres ps01 (psexp) (payload))
(defpres ps02 (psexp) (payload))
(defpres ps03 (psexp) (payload))
(defpres ps04 (psexp) (payload))
(defpres ps05 (psexp) (payload))
(defpres ps06 (psexp) (payload))
(defpres ps07 (psexp) (payload))
(defpres ps08 (psexp) (payload))
(defpres ps09 (psexp) (payload))

(defpres p-atom (pres) (payload))
(defpres p-unk  (pres) (payload))

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
  
  (pres-tag pbuf ps00  (:foreground "#FFCC33" :editable t) )
  (pres-tag pbuf ps01  (:foreground "#CCFF33" :editable t) )
  (pres-tag pbuf ps02  (:foreground "#33FF66" :editable t) )
  (pres-tag pbuf ps03  (:foreground "#FF6633" :editable t) )
  (pres-tag pbuf ps04  (:foreground "#33FFCC" :editable t) )
  (pres-tag pbuf ps05  (:foreground "#FF3366" :editable t) )
  (pres-tag pbuf ps06  (:foreground "#33CCFF" :editable t) )
  (pres-tag pbuf ps07  (:foreground "#FF33CC" :editable t) )
  (pres-tag pbuf ps08  (:foreground "#CC33FF" :editable t) )
  (pres-tag pbuf ps09  (:foreground "#3366FF" :editable t) )
  
  (pres-tag pbuf p-unk   (:foreground "yellow" :editable t) (:left-gravity t))
  (pres-tag pbuf p-atom  (:foreground "green" :editable t) )
  ;;----------------------------------------------------------------------------
  ;; I must apologize for the clumsiness, but presentations are derived from
  ;; a gobject class, and closer-mp:class-prototype fails here (is it portable?)
  ;; Anyway, yes, create an instance for the sole purpose of filling its class
  ;; keymap slot.
  (let ((temp (make-instance 'kpres)))
    (clrf (keymap temp))
    (keymap-def (keymap temp) (kbd "C-x C-c") (lambda () (format t "YESSSSSS!!!!~&")) )
    (format t "FUCK ~A" (keymap temp)))
  
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
