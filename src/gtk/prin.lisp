(in-package :subtext)
;;------------------------------------------------------------------------------
;;==============================================================================
 
;;=============================================================================
;; PRIN - SubText printing facility
;;
;; prin stream &rest texts
;;
;; stream--- a stream
;; texts--   one or more of
;; * atom, which is simply printed;
;; (tag &rest texts) --- texts are tagged
;; ((pres) &rest texts) -- texts are printed inside presentation
;;
;; Tag = "tagname" or gtk-text-tag
;;
;; pres = form evaluated to create a presentation
;;
;; Examples:
;; (prin out "ok," (tag1 " Mr. Wise Guy") "!")
;; (prin out (tag1 "hello" (tag2 " cruel") " world")); nested tags
;; (prin out (tag1 "hello" (+ 1 2)))); generate output
;; (prin out (tag1 "hello" (pres (init) " whatever"); create a presentation


(defun prin-texts (stream forms)
  "print the forms to the stream, using tags and presentations"
  (loop for form in forms do
       (if (consp form) 
	   (if (consp (car form)) ;double is ((pres ..) ..)
	       (if (consp (caar form)) ;(((..))) just evaluate
		   (princ (eval (caar form)))
		   ;; presentation ((pres ..) ..)
		   (prin-pres stream  form))
	       ;; tag (tag ..)
	       (prin-tag stream form))
	   ;;atom
	   (princ form stream))) )

(defun prin-tag (stream tagform)
  (let ((promise (make-promise
		  :start (file-position stream)
		  :content (car tagform))))
    (prin-texts stream (cdr tagform))
    (setf (promise-end promise) (file-position stream))
    (push promise (promises stream))))

;; ((pres ..) ..)
(defun prin-pres (stream presform)
 ;; (format t "APPLY #'MAKE-INSTANCE ~A~&" (car presform))
  (let ((promise (make-promise
		  :start (file-position stream)
		  :content (eval (car presform)))))
    (prin-texts stream (cdr presform))
    (setf (promise-end promise) (file-position stream))
    (push promise (promises stream)))  )


(defmacro prin (stream &rest texts )
  "print the texts to the gtk stream, using tags and presentations"
  `(prin-texts ,stream ',texts ))
