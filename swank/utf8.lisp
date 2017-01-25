(in-package :swa)

(defun utf8-encode (string &key (start 0) (end (length string)))
  "Convert a lisp string to UTF"
  (declare (type string string)
           (type fixnum start end))
  #+allegro
  (excl:string-to-octets string :start start :end end :null-terminate nil :external-format :utf8)
  #+ccl
  (ccl:encode-string-to-octets string :start start :end end :external-format :utf-8)
  #+clisp
  (ext:convert-string-to-bytes string charset:utf-8 :start start :end end)
  #+sbcl
  (sb-ext:string-to-octets string :start start :end end :external-format :utf-8)
  #-(or allegro ccl clisp sbcl)
  (trivial-utf-8:string-to-utf-8-bytes (subseq string start end)))


(defun utf8-decode (vector &key (start 0) (end (length vector)))
  "Convert a UTF8 string to lisp"
  (declare (type (vector (unsigned-byte 8)) vector)
           (type fixnum start end))
  #+allegro
  (excl:octets-to-string vector :start start :end end :external-format :utf8)
  #+ccl
  (ccl:decode-string-from-octets vector :start start :end end :external-format :utf-8)
  #+clisp
  (ext:convert-string-from-bytes vector charset:utf-8 :start start :end end)
  #+sbcl
  (sb-ext:octets-to-string vector :start start :end end :external-format :utf8)
  #-(or allegro ccl clisp sbcl)
  (trivial-utf-8:utf-8-bytes-to-string (subseq vector start end)))
