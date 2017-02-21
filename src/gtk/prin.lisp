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
;;
;; pres = form evaluated to create a presentation
;;
;; Examples:
;; (prin out "ok," (tg tag1 " Mr. Wise Guy") "!")
;; (prin out (tg "blue "hello" (tg tag2 " cruel") " world")); nested tags
;; (prin out (tag1 "hello" (+ 1 2)))); generate output
;; (prin stream 1 2 (tg "blue" "hello") (pr button (:code ...) "whatever"))

(defun pr-output (stream list)
;;  (format t "~A~&" list)
  (let ((promise (make-promise
		  :start (file-position stream)
		  :content (first list))))
    (loop for item in (cdr list) do
	 (typecase item
	   (cons (pr-output stream item))
	   (t (princ item stream))))
    (setf (promise-end promise) (file-position stream))
    (push promise (promises stream))))



(defun tg (tag &rest rest)
  `(,tag ,@rest))
(defmacro pr (class init &rest rest)
  `(list (make-instance ',class ,@init) ,@rest) )
(defmacro prin (stream &rest rest)
  (let ((params `(tg "normal" ,@rest)))
    `(pr-output ,stream ,params))
  )

