(in-package :subtext)
;;; Keystroke
;;;
;;;
;;; Naming conventions (a little confusing, bear with me)
;;;
;;; gtkkey  a GTK key code from the event
;;; keyname a GTK name for a gtkkey
;;; key -   a gtk key code with modifiers in high bits
;;; keyseq  a list of keys
;;; keystr  an emacs-like string that parses to a key

(defun split-seq (seq &key (separators " ") (test #'char=) (default-value '("")))
  "split a sequence into sub sequences given the list of seperators."
  (labels ((sep (char) (cl:find char separators :test test)))
    (or (loop for i = (position-if (complement #'sep) seq)
	   then (position-if (complement #'sep) seq :start j)
	   as j = (position-if #'sep seq :start (or i 0))
	   while i
	   collect (subseq seq i j)
	   while j)
	default-value)))
;;-----------------------------------------------------------------------------
;; symbol mapping
;;
;; keep a cache mapping symbol->value and back for each key used...

(defparameter *keyname->gtkcode* (make-hash-table :test #'equal))
(defparameter *gtkcode->keyname* (make-hash-table))

(defun keycache-set (name gtkcode)
  "definitively add keyname and value mapping in the cache; return val"
  (let ((keyname (copy-array name)) ; a clean string used in both hashtables
	(gtk-code (key-gtkcode gtkcode))); TODO: error check range
    (setf (gethash gtk-code *gtkcode->keyname*) keyname
	  (gethash keyname *keyname->gtkcode*) gtk-code)))


(defun keyname->gtkcode (name &optional start)
  (let ((keyname (if start (subseq name start) name)))
    (or (gethash keyname *keyname->gtkcode*)
	(let ((val (gdk-keyval-from-name keyname)))
	  (when (= #xFFFFFF val)
	    (error "keyname->gtkcode cannot find key <~A>~&" keyname))
	  (keycache-set keyname val)))))

(defun key->keyname (key)
  "convert a gtkcode to a keyname, ignoring modifiers.  Error if cannot."
  (let ((val (key-gtkcode key)))
    (or (gethash val *gtkcode->keyname*)
	(let ((name (gdk-keyval-name val)))
	  ;; if name NIL, or "0x...", not found...
	  (if (or (not name) ; nil means not mapped by gdk, but bindings return "0x.."
		  (and (> (length name) 1)	; only "0" starts with 0
		       (char= #\0 (char name 0))))
	      (error "gtkcode->keyname cannot find key <~A>~&" val))
	  (keycache-set name val)
	  name))))

(defun keycache-init-pair (our gtk)
  (let ((keyval (gdk-keyval-from-name gtk)))
    (when (= keyval #xFFFFFF)
      (error "keycache-init-pair: cannot find a gtk key ~S" gtk))
    (keycache-set our keyval)))
(defun keycache-init-list (pairlist)
  (when pairlist
    (keycache-init-pair (first pairlist) (second pairlist))
    (keycache-init-list (cdr (cdr pairlist)))))


;;-----------------------------------------------------------------------------
;; Our keys are 24 bits of value, and 8 bits of modifiers...
;;
(deftype key () '(integer 0 #x3FFFFFFF))

(defparameter gtkcode-spec (byte 24 0))
(defparameter keymod-spec (byte 6 24))

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



(defun key-mod-chars (key)
  "create a list containing modifier characters of key"
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type fixnum key))
  (let ((chars "CMASsH"))
    (loop for i fixnum from 0 upto 5
       when (logbitp (+ 24 i) key)
       collect (char chars i))))




(declaim (inline key-gtkcode key-mod))
;;-----------------------------------------------------------------------------
(defun key-gtkcode (key)
  "return gtkcode of the key, stripping modifier bits"
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type fixnum key))
  (ldb #.'(byte 24 0) (the fixnum key)))
;;-----------------------------------------------------------------------------
(defun key-mod (key)
  "return mod flags of key, not settable"
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type fixnum key))
  (mask-field #.'(byte 6 24) key))

;;------------------------------------------------------------------------------
;; Fuck emacs.  Key strings are names of keys separated by space(s), with an
;; optional prefix.  So, "C-x C-k" or "C-F12 Return" are OK.

(defun key-write (key stream)
  (format stream "~{~A-~}~A" (key-mod-chars key) (key->keyname key)))

(defun keyseq-write (keyseq stream)
  (loop for key in keyseq do
       (key-write key stream)))

(defun gtkmods-subject (keyval gtkmods unshiftable)
  "subject keyval to gtkmods"
  (dolist (modifier gtkmods)
    (case modifier
      (:shift-mask (unless unshiftable (incf keyval MOD-SHIFT-MASK)))
      (:control-mask (incf keyval MOD-CONTROL-MASK))
      (:mod1-mask    (incf keyval MOD-META-MASK))))
  keyval)

(defun char->modmask (char)
  "return a modmask for a character.  If not a mask character, error"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (case char
    (#\S mod-shift-mask)
    (#\C mod-control-mask)
    (#\A mod-alt-mask)
    (#\M mod-meta-mask)
    (#\s mod-super-mask)
    (#\H mod-hyper-mask)
    (t (error "char->modmask: ~C is not a valid modifier prefix" char) )))
#||
(defun char->modmask (char)
  (declare (optimize (speed 3) (debug 0) (safety 0))
	  )
  (let ((pos (position char "CMASsH")))
    (or  (and pos
	      (ash #x1000000 pos))
	 (error "char->modmask: ~C is not a valid modifier prefix" char))))
||#

;;; The shift handling is a shitshow.  Some keystrokes (alpha) have different
;;; keyval if shift is present - making assumptions... Others do not.  We want
;;; to be able to handle things like S-Delete... But S-a or A?
;;;
;;; So if keyval converts to a unicode character, AND is a graphic character,
;;; we shall ignore SHIFT.  For all other characters, we will honor it.
;;; SHIFT-LOCK should be ignored.
;;;
(defun key-make (val &optional (gtk-modifier-list nil)) 
  "create a key using the gtk modifier list"
  (gtkmods-subject val gtk-modifier-list ;if printable, unshiftable.
		   (graphic-char-p (code-char val))))


;;; A key is represented by a string by one of:
;;; - a single character i.e. "A"
;;; - a valid GTK character name, like Return
;;; - optionally, preceded one or more dash-separated modifier S-Return


(defun key-parse (str &optional (result 0)  (off 0) (len (length str)))
  (let* ((off1 (+ off 2)))
    (if (and (< off1 len)
	     (char= #\- (char str (1+ off))))
	(key-parse str (+ result (char->modmask (char str off))) off1 len)
	(+ result (keyname->gtkcode str off)))))

(defun kbd (string)
   "Read a string containing key descriptions, and "
  (mapcar #'key-parse (split-seq string)))

(defun key-is-modifier (gtkcode)
  "check if the gtkcode is a modifier (shift, etc)"
  (and (>= gtkcode #xffe1)
       (<= gtkcode #xffee)))


;;
;;------------------------------------------------------------------------------
;; Global initialization
;;
;; single-character keys are useful...
(defun eli-initialize ()
  (keycache-init-list
   '("!" "exclam"      "@" "at"            "#" "numbersign"  "$" "dollar"
     "%" "percent"     "^" "asciicircum"   "&" "ampersand"   "*" "asterisk"
     "(" "parenleft"   ")" "parenright"    "-" "minus"       "_" "underscore"
     "+" "plus"        "=" "equal"         ":" "colon"       ";" "semicolon"
     "\"" "quotedbl"   "'" "apostrophe"    "{" "braceleft"   "}" "braceright"
     "[" "bracketleft" "]" "bracketright"  "|" "bar"         "\\" "backslash"
     ";" "semicolon"   ":" "colon"         "~" "asciitilde"  "`"  "grave"
     "Mouse-1" "Pointer_Button1"      "Mouse-2" "Pointer_Button2"
     "Mouse-3" "Pointer_Button3"      "Mouse-4" "Pointer_Button4"
     "Mouse-5" "Pointer_Button5")))
