(asdf:defsystem #:stext
  :description "subtextations using gtk3 text editor"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "MIT"
  :depends-on (#:cl-cffi-gtk      
	       #:bordeaux-threads
	       #:trivial-gray-streams
	       #:usocket
	       #:anaphora
	       #:alexandria
	       #-(or allegro ccl clisp sbcl) ;for systems with no Unicod
               #:trivial-utf-8
	       )
  :serial t
  :components (
	       
               (:file "package")
	       (:file "stext")

	       (:file "eli/keysyms")
	       (:file "eli/keystroke")
	       (:file "eli/keymap")

	       (:file "swank/swank-protocol")
	       (:file "swank/utf8")

               (:file "range/package")
               (:file "range/range")

	       (:file "stext/wtxt")
	       (:file "stext/gtbstream")
	       (:file "stext/rbuffer")
	       (:file "stext/rstream")

	       (:file "stext/modeline")
	       (:file "stext/window")

	       (:file "stext/minibuf")
	       (:file "stext/frame")
	       
;;	       (:file "stext/gtk-stream")
;;	       (:file "stext/gtk-in-stream")
	  

;;	       (:file "stext/rview")
;;	       (:file "stext/rbuffer")
;;	       (:file "stext/bars")
;;	       (:file "stext/window")
;;	       (:file "stext/frame")
	       
;;	       (:file "stext/term")
;;	       (:file "stext/redit")
;;	       (:file "stext/sldb")
	       ;;	       (:file "stext/types")
	       (:file "stext/sldb")
	       (:file "swank/swank-parser") ;it knows about tags...
	       (:file "stext/swarepl")
             (:file "stext/gtk-ui")

	      ; (:file "stext/cline1")
	;;       (:file "bnn/edition")
	;;       (:file "bnn/group")
	       ))

