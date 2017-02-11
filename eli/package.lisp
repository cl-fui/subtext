(defpackage #:eli
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo :cffi
	#:cl #:alexandria)
  (:shadow cl:find )
  (:export
   ;; keymap-----------------
   :keymap-make
   :keydef    ; key in keymap
   :key-lookup ;a single key in a single keymap binding
   :key-find   ;a keysequence in a keymap tree
   ;; keystroke
   :key-is-modifier ;is it a gtk modifier key?
   :key-make ; from a gtk event
   :key-write
   :keyseq-write
   :kbd      ; parse a string
   :initialize
   ))










