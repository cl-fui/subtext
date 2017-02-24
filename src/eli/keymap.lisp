(in-package :subtext)


(defclass keymap ()
  ((lst :accessor lst :initform nil)))


(defun keymap-def (keymap keyseq action)
  (push (cons (make-array (length keyseq) :initial-contents keyseq)
	      action)
	(lst keymap)))

(defun keymap-dump (keymap)
  (loop for binding in (lst keymap) do
       (keyseq-write (car binding) *standard-output*)
       (format t ":~A~&" (cdr binding))))

;; Tricky: there is a difference between not finding any possible match and
;; finding a partial match.  As we type, we repeately match; if no possible
;; match exists, the command must be canceled.
(defun keymap-find (keymap keyseq)
  "try to find a binding for a keyseq."
  (let ((partials 0)
	(len (length keyseq)))
    (labels
	((findit (list keyseq)
	   ;; (format t "FINDIT ~A ~A~&" list keyseq)
	   (and list
		(let ((mismatch (mismatch (caar list) keyseq)))
		  (if mismatch
		      (progn
			(and (= len mismatch) (incf partials))
			(findit (cdr list) keyseq))
		      (progn
			(incf partials)
			(car list)))))))
      (values (findit (lst keymap) keyseq) partials))))




