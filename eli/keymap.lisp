(in-package :stext)

;;; Key bindings are stored in keymaps.
;;;
;;; We associate stringified keyseq like "<M-C-x><C-k>hello" with bvals,
;;; which may be symbols or functions.
;;;
;;; A basic binding is ( key . data )
;;; A keymap binding is (key . ALIST)
;;; A context is a cons like that above, used to preserve a
;;; position in a search...The cdr is either a final value or
;;; another context...

(defun keymap-make ()
  "create a new, empty keymap"
  (list nil))
;;----------------------------------------------------------------
;; To define a new keysequence, recursively follow context,
;; creating nodes as needed.  If bindings exist, reuse keys
;; destructively replacing them.
(defun key-def (context keyseq data)
  (let* ((key (car keyseq))
	 ;;match is a binding with our key
	 (match (or (assoc key (cdr context) :test #'equal)
		    (let ((new (cons key nil)))
		      (push new (cdr context))
		      new))))
    (let ((remaining (cdr keyseq)))
      (if remaining; more contexts, otherwise, store data.
	  (key-def match remaining data)
	  (setf (cdr match) data))))) 


;; Note: should assoc not find the next context, we wind up recursing with a
;; nil context for the rest of the search, resulting in a nil... Not finding
;; a key is not a rush job, so who cares...
(defun key-find (context keyseq)
  (if keyseq
      (key-find (assoc (car keyseq) (cdr context)) (cdr keyseq))
      context))

(defun key-lookup (context key)
  "lookup a single key in a binding"
  (assoc key (cdr context))
  )








