(in-package :subtext)

;; Keymap is a list of bindings!



(defmacro keymap-def (keymap keyseq action)
  (let ((seq (gensym)))
    `(let ((,seq ,keyseq))
       (push (cons (make-array (length ,seq) :initial-contents ,seq)
		   ,action)
	     ,keymap))))

(defun keymap-dump (keymap)
  (loop for binding in  keymap do
       (keyseq-write (car binding) *standard-output*)
       (format t ":~A~&" (cdr binding))))

;; Tricky: there is a difference between not finding any possible match and
;; finding a partial match.  As we type, we repeately match; if no possible
;; match exists, the command must be canceled.
(defun keymap-find (keymap keyseq &optional (partials 0))
  "try to find a binding for a keyseq.  If found, return binding; if not,
return number of partial matches.  If no partials, return nil"
  (let ((len (length keyseq)))
    (labels
	((findit (list keyseq)
	   ;; (format t "FINDIT ~A ~A~&" list keyseq)
	   (and list
		(let ((mismatch (mismatch (caar list) keyseq)))
		  (if mismatch
		      (progn
			(and (= len mismatch) (incf partials))
			(findit (cdr list) keyseq))
		      (progn; full match!
			(incf partials)
			(car list)))))))
      (values (findit keymap keyseq) partials))))




