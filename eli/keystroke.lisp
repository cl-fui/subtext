(in-package :stext)
;;; Keystroke
;;;

(defconstant MOD-CONTROL-BIT 24)
(defconstant MOD-META-BIT    25)
(defconstant MOD-ALT-BIT     26)
(defconstant MOD-SHIFT-BIT   27)
(defconstant MOD-SUPER-BIT   28)
(defconstant MOD-HYPER-BIT   29)

(defconstant MOD-CONTROL-MASK (ash 1 MOD-CONTROL-BIT) )
(defconstant MOD-META-MASK    (ash 1 MOD-META-BIT) ) 
(defconstant MOD-ALT-MASK     (ash 1 MOD-ALT-BIT) ) 
(defconstant MOD-SHIFT-MASK   (ash 1 MOD-SHIFT-BIT) ) 
(defconstant MOD-SUPER-MASK   (ash 1 MOD-SUPER-BIT) ) 
(defconstant MOD-HYPER-MASK   (ash 1 MOD-HYPER-BIT) )


(deftype key () '(integer 0 #x3FFFFFFF))

(defparameter keyval-spec (byte 24 0))
(defparameter keymod-spec (byte 8 24))
;;; key is not a struct, but we never set it.  Usually, we start with 0
;;; and addf masks and values.  These accessors are macros:
(defmacro key-val (key)
  "return keyval of the key"
  `(ldb keyval-spec ,key))
(defmacro key-mod (key)
  "return mod flags of key, not settable"
  `(mask-field keymod-spec ,key))

(defun key->character (key)
  "return a Lisp character corresponding to this key"
  ;;;TODO: fix this
  (and (<  (key-val key) char-code-limit)
       (code-char (key-val key))))

(defun write-key (key stream)
  (let* ((name (gtkcode->gtkname (key-val key)))
	 (brace (or (not (zerop (key-mod key)))
		    (> (length name) 1))))
    (when brace (write-char #\< stream))
    "print the stringified key to stream"
    (when (logbitp mod-control-bit key) (write-sequence "C-" stream ))
    (when (logbitp mod-meta-bit    key) (write-sequence "M-" stream ))
    (when (logbitp mod-alt-bit     key) (write-sequence "A-" stream ))
    (when (logbitp mod-shift-bit   key) (write-sequence "S-" stream ))
    (when (logbitp mod-super-bit   key) (write-sequence "s-" stream ))
    (when (logbitp mod-hyper-bit   key) (write-sequence "H-" stream ))
    (write-sequence name stream )
    (when brace (write-char #\> stream)))
  key)

(defun write-keyseq (keyseq stream)
  (loop for key across keyseq do
       (write-key key stream)))
  
(defun keyseq->string (keys)
  (with-output-to-string (s) (write-keyseq keys s)))

(defun key->string (key)
  "Convert a key into a string representation"
  (with-output-to-string (s) (write-key key s)))

;;; Associate chars used as modifiers in command-strings to modmasks
;;; ah, defconstant reference in a quoted form seems to be a symbol, not value
(defparameter *char-modmask* `((#\C . ,mod-control-mask)
			       (#\M . ,mod-meta-mask)
			       (#\A . ,mod-alt-mask)
			       (#\S . ,mod-shift-mask)
			       (#\s . ,mod-super-mask)
			       (#\H . ,mod-hyper-mask)))
(defun char->modmask (char)
  "return a modmask for a character.  If not a mask character, error"
  (or (cdr (assoc char *char-modmask*))
      (error 'eli-error :message "char->modmask: char is not a mask-appropriage"
	      :value char)))
;;;
;;; We only care about control and meta (alt key).
;;; Shift is already processed for us.
(defun make-key (val &optional (gtk-modifier-list nil)) 
  "create a key using the gtk modifier list"
  (dolist (modifier gtk-modifier-list)
    (case modifier 
      (:control-mask (incf val MOD-CONTROL-MASK))
      (:mod1-mask    (incf val MOD-META-MASK))))
  val)


;;;
;;; A gtkkey is represented bye a string by one of:
;;; - a single character i.e. "A"
;;; - a valid gtkname inside angle braces like <RET> (see keysyms.lisp)
;;; - a valid gtkname inside angle braces, preceded by a modifier like
;;;     "<M-RET>
;;; Pay attention to lisp escapes, and additional escape for < outside <>:
;;; "<>>" is >
;;; "<\"" is "
;;; "hello\\<world>" is "hello<world" (extra escape for <)
;;; "<<>" is <
;;; "\\<" is also <

(defun key-reader (stream &key (buf (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)))
  "read a textual character representation like <C-M-x> and return key"
  (let ((key 0)
	(c (read-char stream)))
    (setf (fill-pointer buf) 0)
    (case c
      (#\\ (vector-push (read-char stream) buf)) ;escapes in 
      (#\< (loop for i upto 80 
	      for c1 = (read-char stream)
	      for c2 = (peek-char nil stream)
	      while (char= #\- c2) do
		(if (char= #\> c1)     ;prevent <> or <C-> type errors
		    (error "unexpected >" ))
		(read-char stream)	;skip the -
		(incf key (char->modmask c1))
	      finally (vector-push-extend c1 buf)) ;overshot, keep the char
	   ;; now collect the rest of the buf
	   (loop for c = (read-char stream)
	      if (char= c #\\) do
		(setf c (read-char stream))
	      until (char= c #\>) do
		(vector-push-extend c buf)))
      (t (vector-push c buf))) ;not escape, 
    (let ((gtkcode (gtkname->gtkcode buf)))
      (if gtkcode
	  (incf key gtkcode)
	  (error "~A is not a valid gtk key name" buf)))
    key))

(defun kbd (string)
  "parse a string into a _list_ of gtkkeys, using <M-x>hello notation"
  (with-input-from-string (in string)
    (let ((buffer (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t) ))
      (loop while (listen in)
	 collect (key-reader in :buf buffer)))))




;;; move to dead-code.
#|
(defun key-reader-old (stream char)
  "read textual character representations like <C-M-x> and return keys"
  (declare (ignore char))
  (let ((key 0)
	(name (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for i upto 80 
       for c1 = (read-char stream)
       for c2 = (peek-char nil stream)
       while (char= #\- c2) do
	 (if (char= #\> c1) ;prevent <> or <C-> type errors
	     (error "unexpected >" ))
	 (read-char stream) ;skip the -
	 (incf key (char->modmask c1))
       finally (vector-push-extend c1 name))
    ;; now collect the rest of the name
    (loop for c = (read-char stream)
       until (char= c #\>) do
	 (vector-push-extend c name))
    (let ((gtkcode (gtkname->gtkcode name)))
      (if gtkcode
	  (incf key gtkcode)
	  (error "~A is not a valid gtk key name" name)))
    key))

(defun parse-gtkkey-p (str start end key)
  "parse the gtkkey fragment of a string return updated key"
  (let ((k (gtkname->gtkcode  (subseq str start end))))
    (unless k  (error 'eli-error :message "parse-key: invalid string" :value str))
    (+ key k)))

(defun parse-gtkkey-1 (str index)
  "parse a string for a key description, and return key and index"
  (let ((c (elt str index))
	(key 0))
    (if (char= c #\<)
	(progn
	  (incf index 1) ;skip <
	  (loop for i upto 10
	     for c1 = (elt str index)
	     for c2 = (elt str (1+ index))
	     while (char= #\- c2) do
	       (incf index 2) ;skip the modifier pair
	       (incf key (char->modmask c1)))
	  ;;find ending >
	  (let ((end (position #\> str :start (1+ index))))
	    (values (parse-gtkkey str index end key) (1+ end))))
	(values (parse-gtkkey str index (1+ index) key) (1+ index)))))
;(set-macro-character #\< #'key-reader)
|#





