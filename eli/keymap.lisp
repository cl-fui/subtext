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

(defun keymap-define-key (binding gtkkey data)
  "append keymap with a key"
  (let ((val (cons gtkkey data)))
    (push val (cdr binding) )
    val))

(defun keymap-bindp (binding keylist data)
  (if keylist; more keys to come?
      (keymap-bindp
       (or (keymap-lookup binding (car keylist)) ;try to find this key
	   (keymap-define-key binding (car keylist) nil))
       (cdr keylist) data)
      (setf (cdr binding) data)))

(defun keymap-bind (binding keystr data)
  (keymap-bindp binding (kbd keystr) data))



(defun keymap-bind-self-insert (keymap gtkkey stream)
  (flet ((self-inserter (gtkkey) ;closes over stream, but not key
	   (write-char (key->character gtkkey) stream)))
    ;;TODO: check parameters
    (keymap-define-key  keymap gtkkey #'self-inserter)))



(defun keymap-bind-self-keys-in-str (string keymap stream)
  (flet ((self-inserter (gtkkey) ;closes over stream, but not key
	   (write-char (key->character gtkkey) stream)))
    ;;TODO: check parameters
    (let ((s (make-string 1)))
      (loop for char across string do
	   (setf (char s 0) char)
	   (keymap-define-key 
	    keymap (symbol-value (find-symbol s :kp))
	    #'self-inserter)))))











