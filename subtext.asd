(asdf:defsystem #:subtext
  :description "text presentations using gtk3 text editor"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD"
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
               (:file "src/package")
	       (:file "src/utils")


	       (:file "src/eli/keystroke")
	       (:file "src/eli/keymap")
	       
	       (:file "src/gtk/tb")
	       (:file "src/gtk/simplestream")
	       (:file "src/gtk/echostream")
	       (:file "src/gtk/termstream")
	       (:file "src/gtk/pres")
	       (:file "src/gtk/promises")
	       (:file "src/gtk/prin")


	       (:file "src/eli/eli"); here because elie is now buffer...
	       

	       
	       (:file "src/gtk/rview")
	       (:file "src/gtk/window")
	       (:file "src/gtk/frame")

	       (:file "src/gtk/modeline")
	       (:file "src/gtk/minibuf")

	       (:file "demo/demo")
	       (:file "demo/repl/swank/swank-protocol")
	       (:file "demo/repl/swank/utf8")
	       (:file "demo/repl/sldb")
	       (:file "demo/repl/swarepl")

	       (:file "demo/news/edition")
	       (:file "demo/news/group")
	       (:file "demo/news/news")
	       (:file "src/gtk/gtk-ui")
	       ))

