(in-package :subtext)

(defcontext pnormal (ctx) ())

(defcontext psexp (ctx)  (payload))
(defkeymap psexp)
;(defparameter keymap-psexp nil)
;(defmethod keymap ((it psexp))  keymap-psexp)
;; TODO: obviously, there should be a way to programmatically create context
;; classes!  What does it entail?  It's harder than it seems, as defclass does
;; weird things at weird times...

;;OK (ql:quickload :stext)(in-package :stext)



;;==============================================================================
;; Define 30 context types for  parenthesized sexp levels...
(defmacro def-numbered-level (number)
  (let* ((num number)
	 (classname (intern (format nil "PS~A" num))))
    `(defcontext ,classname (psexp) ((level :accessor level :initform ,num :allocation :class)))))


(print (loop for i from 0 to 29 collect ( eval `(def-numbered-level ,i))))

 

(defcontext p-atom (ctx) (payload))
(defcontext p-unk  (ctx) (payload))

;;;=============================================================================
;;;
(defclass buflisp (conbuf)
  ((level   :accessor level   :initform (make-array 30)) ;contains classes
   (pindex  :accessor pindex  :initform 0))
  (:metaclass gobject-class))

(defmethod initialize-instance :after ((pbuf buflisp) &key )
  (print "initialize-instance: buflisp")
  (setf *pbuf* pbuf);***
  ;; for now keep array of sexp classes.  Some deep magic requires
  ;; eval to be involved here... I wish I understood this better.
  (setf (level pbuf)
	(make-array 30 :initial-contents
		    (loop for i from 0 to 29
		       collect (eval `(def-numbered-level ,i)))))
  ;;---------------------------------------------------------------------------
  ;; Associate presentations to this buffer.  Priority is important!

  (pbuf-new-tag pbuf :name "normal"  :foreground "AntiqueWhite1" :editable t)

  (let ((colors
	 (make-array 10 :initial-contents
		     '("#FFCC33" "#CCFF33" "#FF6633" "#33FFCC" "#FF3366"
		       "#33CCFF" "#FF33CC" "#CC33FF" "#3366FF" "#FFCC33" ))))
    (loop for i from 0 to 29
       for color = (aref colors (mod i 10))
       for classname = (class-name (aref (level pbuf) i))
       do(print (eval `(context-tag ,pbuf ,classname (:foreground ,color))))))
  ;  (context-tag pbuf ps29  (:foreground "#3366FF" :editable t) )
  ;; Technically, the following will never overlap, and therefore, priority should
  ;; not matter...
  (context-tag pbuf p-atom  (:foreground "green" :editable t) )
  (context-tag pbuf p-unk   (:foreground "yellow" :editable t) (:left-gravity t))
 
  ;;----------------------------------------------------------------------------


  (clrf keymap-psexp)
  (keymap-def keymap-psexp (kbd "C-x C-c") (lambda () (format t "YESSSSSS!!!!~&")) )
  
   
  (prin pbuf (pr 'p-unk () "  ")))



(defmethod -on-announce-eli :after ((pbuf buflisp) eli)
  (print "on-announce-eli")
  (with-slots (pindex level) pbuf
    (eli-def
     eli (kbd "(")
     (lambda (); (format t ">..~A ~A~&" parens pindex)
       (prin pbuf (ctx (aref level pindex) ()  "()"))
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
      (prin buffer "hello " (pr 'ps0 () "p0..." (pr 'ps1 () "p1...") "p0 again"
				(pr 'ps2 () ".2.." (pr 'ps3 () "3/")) "and 0") "---")
))
  )
()
