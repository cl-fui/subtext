;;;; package.lisp

(defpackage #:stext
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo :cffi
	;;:eli
	:alexandria  :anaphora
	#:cl
))

