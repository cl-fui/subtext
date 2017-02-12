(defpackage #:eli
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo :cffi
	#:cl #:alexandria)
  (:shadow cl:find )
  (:export
   :eli :make-eli
   :def
   :find
   ;; keymap-----------------
   :keymap-make
   :key-lookup ;a single key in a single keymap binding
   ;; keystroke
   :key-is-modifier ;is it a gtk modifier key?
   :key-make ; from a gtk event
   :key-write
   :keyseq-write
   :kbd      ; parse a string
   :initialize
   :process-key
   ))










