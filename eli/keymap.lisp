(in-package :stext)

;;; Key bindings are stored in keymaps.
;;;
;;; We associate stringified keyseq like "<M-C-x><C-k>hello" with bvals,
;;; which may be symbols or functions.
;;;
;;; (key . data) (key . data
;;;
;;; 


(defun keymap-make () (cons 0 nil))

(defun keymap-dump (keymap)
  (format t "(~A " (key->string (car keymap)))
  (if (consp (cdr keymap))
      (loop for i in (cdr keymap) do
	   (keymap-dump i))
      (format t "~A" (cdr keymap)))
  (format t ") "))

(defun keymap-lookup (binding key)
  (when binding
    (let ((list (cdr binding))) 
      (if (eql key (car (first list)))
	  (first list)
	  (keymap-lookup list key)))))
(defun keymap-find (binding key)
  (cdr (keymap-lookup binding key)))

(defun keymap-define-key (binding key data)
  (let ((val (cons key data)))
    (push val (cdr binding) )
    val))

(defun keymap-bindp (binding keylist data)
  (if keylist; more keys to come?
      (keymap-bindp
       (or (keymap-lookup binding (car keylist)) ;try to find this key
	   (keymap-define-key binding (car keylist) nil))
       (cdr keylist) data)
      (setf (cdr binding) data)))

(defun keymap-bind (keymap keystr data)
  (keymap-bindp keymap (kbd keystr) data))

