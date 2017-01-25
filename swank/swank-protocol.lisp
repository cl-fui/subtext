(in-package :common-lisp)
;; Start a swank server in another instance:
;; (ql:quickload :swank-protocol)
;; (setf swank:*configure-emacs-indentation* nil)
;; (swank:create-server :port 5000 :dont-close t)
;;
;; For convenicence, 'sbcl --load swanker-old.lisp
;;
;; We assume a connection per UI buffer, so history is kept here.  History
;; is harder than it looks: the first back just repeats the last one...
;; - after processing rex, null-out history index and set last-rex.
;; - history back/forward on null history, starts with last-rex
(defpackage swa
  (:use :cl)
  (:shadow cl:eval)
  (:export :connection :make-connection
           :hostname
           :port
           :pkg
	   :prompt
	   :rexs
	   :histid)
  (:export :do-log
	   :connect :disconnect
           :send-message-string
           :message-waiting-p

	   :get-historical-line
	   :get-next-id
	   :history-back
	   :history-forward
           :emacs-rex
	   :eval
           :emacs-return-string
	   
           :request-invoke-restart
           :request-throw-to-toplevel
           
	   )
  (:documentation "low-level implementation of a client for the swank protocol."))
(in-package :swa)

;;; prevent reader errors

(eval-when (:compile-toplevel :load-toplevel)
  (swank:swank-require '(swank-presentations swank-repl)))


;;=============================================================================
;; every rex issued is saved along with its execution environment in this
;; structure.  we keep it in an array, and use its index as the continuation 
;; id for swank.  when a return message comes in, we match it up using the id.
;; in addition, this provides a history of forms executed for the ui.
;; note that we store a string representation of the form, to allow gc.
(defstruct rex package form proc)
(defun get-next-id (connection)
  "return the next continuation id"
  (fill-pointer (rexs connection)))


;;=============================================================================
(defclass connection ()
  ((hostname :reader hostname
             :initarg :hostname
             :type string
             :documentation "swank host")
   (port :reader port
         :initarg :port
         :type integer
         :documentation "swank port")
   
   (pkg :accessor pkg
            :initform "COMMON-LISP-USER"
            :type string
            :documentation "the name of the connection's package.")
   (prompt  :accessor prompt
            :initform "CL-USER"
            :type string
            :documentation "prompt to display")

   ;history
   (histid  :accessor histid
            :initform nil
            :type fixnum
            :documentation "history-tracking id")
   (rexid   :accessor rexid
            :initform nil
            :type fixnum
            :documentation "last-processed rex id")
   
   (socket :accessor socket
           :type usocket:stream-usocket
           :documentation "the usocket socket.")

   (state  :accessor state
           :documentation "socket state")
   
   
   (thr    :accessor thr
	   :initform nil
	   :documentation "thread used for listening")
   (rexs   :accessor rexs
	   :initform nil)
)
  (:documentation "a swank protocol connection"))


(defun make-connection (hostname port)
  "create a connection to a remote swank server."
  (make-instance 'connection
                 :hostname hostname
                 :port port))
;;==============================================================================
;; utf-8-safe communication...
;;
;; immediately upon connection, a thread is started to process responses.
;; any forms received that are not processed are sent to fallback
;; (fallback connection forms).
(defun default-fallback (connection forms)
  (declare (ignore connection))
  (format *standard-output* "~%default connection fallback: ~a~%" forms))
(defun connect (connection &optional (fallback #'default-fallback))
  "connect to the remote server and process requests, sending unknown
forms to processor."
  (with-slots (hostname port socket thr rexs histid rexid state) connection
    (let ((sock (usocket:socket-connect hostname
					port
					:element-type '(unsigned-byte 8))))
      (setf socket sock
	    rexs (make-array 100 :adjustable t :fill-pointer 0 )
	    histid nil
	    rexid nil
	    state :connected ;set once!
	    thr
	    (bt:make-thread
	     (lambda ()
	       (let ((stream (usocket:socket-stream socket)))
		 (loop named waiter do
		      (usocket:wait-for-input socket :ready-only t :timeout 0.2)
		    ;; socket may have been destroyed
		      (when (eq state :closing)
			(usocket:socket-close socket)
			;;(print "swank connection closed");;;;
			(return-from waiter))
		      (when (message-waiting-p connection)
			(message-process connection
					 (read-packet stream)
					 fallback)))))))))
  t)

(defun disconnect (connection)
  (with-slots (state) connection
    (setf state :closing)))





;;==============================================================================
;; low-level packetization.
;; stream is a utf8 byte stream.  each message is a utf8 string, prefixed with a
;; 24-bit payload-size count, encoded as 6 ascii bytes.
;;
(defun write-packet (stream string)
  "write a string to a stream, prefixing it with length information for swank."
  (let* ((payload  (utf8-encode string))
	 (paylen (1+ (length payload)) )
	 (prefix (utf8-encode (format nil "~6,'0,x" paylen)))
	 (final (make-array (+ 6 paylen ) :element-type '(unsigned-byte 8)
			    :initial-element 32;(char-code #\newline);;TODO***
			    )))
    (replace final prefix); insert 6 bytes of length
    (replace final payload :start1 6)
    (write-sequence final stream)
;;    (format t "~&>>>[~A]~&" string)
;    (log? "~%-->: [~A][~A]~&" paylen string)
    string))


(defun read-packet (stream)
  "read a swank packet and convert payload to a lisp string."
  (let ((arr (make-array 6 :element-type '(unsigned-byte 8))))
    (read-sequence arr stream)
    (let ((paylen (parse-integer (utf8-decode arr) :radix 16)))
      (setf arr (make-array paylen :element-type '(unsigned-byte 8)))
      (read-sequence arr stream)
;      (log? "~%<--: [~A][~A]~&" paylen (utf8-decode arr))
      (let ((val (utf8-decode arr)))
;;    (format t "~&<<<[~A]~&" val)
	val))))

;;==============================================================================
;; Sending strings (we always receive packets)

(defun send-message-string (connection string)
  "send a message string to a swank connection."
  (let ((stream (usocket:socket-stream (socket connection))))
    (prog1 (write-packet stream string)
      (force-output stream))))

(defun message-waiting-p (connection)
  "t if there's a message in the connection waiting to be read, nil otherwise."
  (usocket:wait-for-input (socket connection)
			     :ready-only t
			     :timeout 0))

;;==============================================================================
;;; rex
;;
;; All rex requests are kept in rex structures in rexs array, along with the
;; lambda to process them.  That is how we handle 'continuation' issues.
;; Null requests are OK.

(defun default-rex-proc (connection reply id);rex-callback
  (declare (ignore connection))
  (format t "~%default swank reply handler for id ~a: ~a~&" id reply))

;;------------------------------------------------------------------------------
;; generic rex requests have the form thrown out
(defun emacs-rex (connection string &key (proc #'default-rex-proc) (thread t))
  (print string)
  (when string
    (with-slots (pkg rexs) connection
      (send-message-string
       connection
       (format nil "(:emacs-rex ~a ~s ~a ~a)"
	       string
	       pkg
	       thread
	       (fill-pointer rexs)))
      (vector-push (make-rex :package pkg :form nil :proc proc) rexs))))
;;------------------------------------------------------------------------------
;; eval in repl requests are saved in rex array.
(defun eval (connection string &optional (proc #'default-rex-proc))
  "request a swank repl eval and return the id"
  (when string
    (with-slots (pkg rexs) connection
      (send-message-string
       connection
       (format nil "(:emacs-rex (swank-repl:listener-eval ~s) ~s :repl-thread ~a)"
	       string pkg (fill-pointer rexs)))
      (vector-push (make-rex :package pkg :form string :proc proc) rexs))))
;;
;;
;;==============================================================================
;; non-rex
;;
;; this one is for read-line...
(defun emacs-return-string (connection string id tag)
  "send a string to the server's standard input."
  (send-message-string
   connection
   (format nil "(:emacs-return-string ~a ~a ~s)" id tag string)))


;;==============================================================================
;; the receiver thread runs this to process messages from swank.
;; message is read, and the sexp is processed as follows:
;; - return messages are matched up with the request, and proc is called.
;; - otherwise, fallback provided by the owner is called as
;; (... connection message)
(defun message-process (connection message-str fallback)
  "process a message string."
   (with-slots (rexs histid rexid) connection
    (let ((message (read-from-string message-str)))
      (case (first message)
	(:return ;match it with the request
	  ;; (:return (ok/abort data) id)
	  (destructuring-bind (reply id) (cdr message)
	    (let ((rex (aref rexs id)))
	      
	      (funcall (rex-proc rex) connection reply id)
	      (setf rexid  id; remember last rex processed
		    histid nil));and reset history 
	    ))
	(t  (funcall fallback connection message)))
      ))
 
)
;;==============================================================================
;; History is surprisingly difficult: first time back, we repeat the last rex,
;; after that, we go to previous.  Because of that we have to maintain an
;; 'pipeline' that catches the first-time attempt to get history without
;; incrementing/decrementing. 
(defun get-historical-line (connection id)
  (rex-form (aref (rexs connection) id)))

(defun history-back (connection)
  "return prev historical string"
  (with-slots (histid rexid) connection
    (if histid		       ;if there is history, decrement
	(when (> histid 0)
	  (setf histid 
		(loop for i from (1- histid) downto 0
		   until (get-historical-line connection i)
		   finally (return i)
		   if (zerop i) return histid)))
	(setf histid rexid)) ;otherwise, start with last one.
    (get-historical-line connection histid)))

(defun history-forward (connection)
  (with-slots (histid rexid) connection
    (if histid
	(let ((upper (get-next-id connection)))
	  (when (> upper (1+ histid))
	    (setf histid ;here, try to increment as we don't need this one...
		  (loop for i from (1+ histid) upto upper
		     until (get-historical-line connection i)
		     finally (return i)
		     if (= i upper) return histid))))
	(setf histid rexid))
    (get-historical-line connection histid)))
