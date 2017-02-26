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
;; * expr evaluated and printed
;; * (tg tagdesc &rest texts) --- texts are tagged
;; * (pr type init &rest texts) -- texts are printed inside presentation
;;
;; Tagdesc = "tagname" or gtk-text-tag
;; type = evaluates to symbol for make-instance
;; init = form evaluated to create a presentation
;;
;; Examples:
;; (prin out "ok," (tg tag1 " Mr. Wise Guy") "!")
;; (prin out (tg "blue "hello" (tg tag2 " cruel") " world")); nested tags
;; (prin out (tag1 "hello" (+ 1 2)))); generate output
;; (prin stream 1 2 (tg "blue" "hello") (pr button (:code ...) "whatever"))
;; (prin out ok null you) ; null does not print. (progn ... nil) works too.

(defun pr-output (stream list)
  ;;  (format t "~A~&" list)
  (labels ((prim (stream list)
	     (let ((promise (subtext::make-promise
		  :start (file-position stream)
		  :content (first list))))
	       (loop for item in (cdr list) do
		    (typecase item
		      (cons (prim stream item))
		      (null)
		      (t (princ item stream))))
	       (setf (subtext::promise-end promise) (file-position stream))
	       (push promise (subtext::promises stream)))))
    (prim stream list)
    (finish-output stream)))


(defmacro prin (stream &rest rest)
  (let ((params `(tg "normal" ,@rest)))
    `(progn 
       (pr-output ,stream ,params)
       )))

(defun tg (tag &rest rest)
  `(,tag ,@rest))

(defmacro pr (class init &rest rest)
  (let ((cl class))
    `(list (make-instance ,cl ,@init) ,@rest)) )

(defmacro ctx (class init &rest rest)
  (let ((cl class))
    `(list (make-instance ,cl ,@init) ,@rest)) )
