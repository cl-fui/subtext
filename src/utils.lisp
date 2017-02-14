(in-package :subtext)

(defun make-synonym (old newname &key (package (symbol-package old)) (overwrite nil))
  "make a synonym for old symbol using newname, optionally in a different package.
 Functions only."
  (unless overwrite
    (when (find-symbol newname package)
      (error "~%Symbol ~A already exists in package ~A." newname package)))
  (let ((new-sym (intern newname package)))
    (setf (symbol-function new-sym) (symbol-function old))
    (export new-sym package)
    new-sym))

(defun abbrev-symbols (package old-prefix new-prefix)
  "Create synonyms starting with 'new-prefix' for function symbols 
starting with 'old-prefix' in :package.  Remember to capitalize "
  (let ((old-prefix-length (length old-prefix)))
    (do-symbols (sym (find-package package))
      (when (fboundp sym) ;functions only
       	(let ((sym-name (symbol-name sym)))
	  (alexandria:when-let (match (search old-prefix sym-name))
	    (when (zerop match)
	      (let* ((abbrev-sym-name
		      (concatenate 'string new-prefix
				   (subseq sym-name old-prefix-length))))
		(make-synonym sym abbrev-sym-name :package package :overwrite t)))))))))

;;(eval-when (:compile-toplevel))
(abbrev-symbols :gtk "GTK-TEXT-VIEW-"      "GTV-")
(abbrev-symbols :gtk "GTK-TEXT-ITER-"      "GTI-")
(abbrev-symbols :gtk "%GTK-TEXT-ITER-"     "%GTI-")
(abbrev-symbols :gtk "GTK-TEXT-BUFFER-"    "GTB-")
(abbrev-symbols :gtk "GTK-TEXT-MARK-"      "GTM-")
(abbrev-symbols :gtk "GTK-TEXT-TAG-"       "GTT-")
(abbrev-symbols :gtk "GTK-TEXT-TAG-TABLE-" "GTTT-")
(abbrev-symbols :gtk "%GTK-TEXT-BUFFER-"   "%GTB-")
(defmacro mvb (&rest rest)
  "synonym for multiple-value-bind"
  `(multiple-value-bind ,@rest))

;;==============================================================================
;;  same as with-gdk-threads-lock!
;;
(defmacro gsafe (&rest body)
  "execute body safely inside gtk main thread"
  `(unwind-protect
	(progn
	  (gdk-threads-enter)
	  ,@body)
     (gdk-threads-leave)))

(defmacro when-setf (place expr &rest rest)
  "when expr is not nil, setf the form and execute optional body"
  (let ((temp (gensym)))
    `(let ((,temp ,expr))
       (when ,temp
	 (setf ,place ,temp)
	 ,@rest))))
;;==============================================================================
;; careful with print methods; they will break everything!
;;
(defmethod print-object ((mark gtk-text-mark) out)
   (print-unreadable-object (mark out :type t)
    (format out "~s" (gtk:gtk-text-mark-name mark))))

(defmethod print-object ((tag gtk-text-tag) out)
   (print-unreadable-object (tag out :type t)
    (format out "~s" (gtk:gtk-text-tag-name tag))))

(defmethod print-object ((iter gtk-text-iter) out)
  (print-unreadable-object (iter out :type t)
    (format out "~s" (gtk:gtk-text-iter-get-offset iter))))

(defmacro idly (&rest rest)
  `(gdk-threads-add-idle (lambda () ,@rest)))




(defun popup-menu (menu-descriptor-list
			     &key (button 0)
			       (activate-time (gtk-get-current-event-time)))
  "Display a popup menu generated from a list of menu-descriptors
'((\"label1\" . handler1)...), and return immediately.  The invoked handler
must take one parameter, the menu-item (which is pretty much useless).
Note: a single underline in label will crate a 'mnemonic'; two __ is _"
  (labels ((unhandled-menu-item (menu-item)
	     (format t "~%UNHANDLED MENU-ITEM ~A"
		     (gtk-menu-item-label menu-item))))
    (let ((menu (gtk-menu-new)))
      (print menu-descriptor-list)
      (loop for desc in menu-descriptor-list do
	   (print desc)
	   (let ((mitem (gtk-menu-item-new-with-mnemonic (car desc))))
	     (g-signal-connect
	      mitem "activate" (if (cdr desc)
				   (eval (cdr desc))
				   #'unhandled-menu-item))
	     (gtk-menu-shell-append menu mitem)))
      (gtk-widget-show-all menu)
      (gtk-menu-popup menu :button button :activate-time activate-time))))

;------------------------------------
(defgeneric -on-destroy         (object))
(defmethod  -on-destroy ((it t)))

(defgeneric -on-initial-display (object))
(defmethod  -on-initial-display ((it t))) ;default



;;; ----------------------------------------------------------------------------
;;; For debugg output use
(defmacro bug (&rest rest)
  `(format t ,@rest))

(defmacro bugx (&rest rest)
  )


#||
(defun achoice (x) (print "FUCK SECOND"))
(defun mtest ()
  (gsafe (popup-menu ("_first" . (lambda (m) (print "FUCK FIRST")))
		     ("s_econd" . #'achoice))))
||#

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_default_attributes ()
;;;
;;; GtkTextAttributes * gtk_text_view_get_default_attributes
;;;                                                    (GtkTextView *text_view);
;;;
;;; Obtains a copy of the default text attributes. These are the attributes used
;;; for text unless a tag overrides them. You'd typically pass the default
;;; attributes in to gtk_text_iter_get_attributes() in order to get the
;;; attributes in effect at a given text position.
;;;
;;; The return value is a copy owned by the caller of this function, and should
;;; be freed.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Returns :
;;;     a new GtkTextAttributes
#|
(in-package :gtk)
(defcstruct gtk-text-attributes
  (:appearance (:pointer (:struct gtk-text-appearance)))
  (:justification gtk-justification)
  (:direction gtk-text-direction)
  (:font (g-boxed-foreign pango-font-description))
  (:font-scale :double)
  (:left-margin :int)
  (:right-margin :int)
  (:indent :int)
  (:pixels-above-lines :int)
  (:pixels-below-lines :int)
  (:pixels-inside-wrap :int)
  (:tabs :pointer)             ; type is pango-tab-array
  (:wrap-mode gtk-wrap-mode)
  (:language (g-boxed-foreign pango-language))
  (:invisible :uint)
  (:bg-full-height :uint)
  (:editable :uint))

(defcfun ("gtk_text_view_get_default_attributes" gtv-get-def-attr) (:pointer (:struct gtk::gtk-text-attributes))
  (text-view (g-object gtk-text-view)))

(defcfun ("gtk_menu_popup_at_pointer" gtk-menu-popup-at-pointer) :void
  (menu g-object)
  (event (g-boxed-foreign gdk-event))
)

|#




