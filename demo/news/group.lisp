(in-package :subtext)


;;;
;;; A group structure, to keep track of a newsgroup on a server...
;;;
(defclass group ()
  ((data   :initarg group-data
	   :accessor group-data)
   (name    :initform "abcc"
	    :accessor group-name)
   (high    :initform 0
	    :accessor group-high)
   (timestamp     :initform 0
	          :accessor group-timestamp)
   (authors-list  :initform nil
		  :accessor group-authors-list) ;upon load, a list of unique author names 
   (authors-count :initform 0
		  :accessor group-authors-count) ;upon load, count of that list))
   ))
;;-----------------------------------------------------------------------------
;; Load from a list of editions.  First, organize into groups of unique
;; posters using a hashtable; replace poster with a reference to the unique one!
   

(defun group-deserialize (stream group)
  "Deserialize all edition objects from the stream..."
  (let ((lines (read stream))
	(authors (make-hash-table :test 'equal :size 100)))
    (with-slots (data high authors-count authors-list) group
      (setf data nil)
      (loop for edition = (read stream nil nil)
	 while edition
	 maximize (edition-max edition) into max do
	   (with-slots (poster) edition
	     ;; Set poster to a unique string; 
	     (setf poster (ensure-gethash poster authors poster))
	     (push edition data))
	 finally (setf high max
		       authors-list (hash-table-keys authors)
		       authors-count (hash-table-count authors ))))
    (format t "~%~D lines read" lines)))

(defun group-load (group)
  (let ((filename
	 (asdf:system-relative-pathname
	  'subtext (uiop:parse-unix-namestring
		    (format nil "demo/news/~A.bnn" (group-name group))))))
 
    (with-open-file (in filename :direction :input)
      (group-deserialize in group)))
  nil)
;;
;; and save...
;;
(defun group-serialize (stream group)
  (print (length (group-data group)) stream)
  (let ((*print-pretty* nil)) ;; minimize whitespace in each line
    (loop for edition in (group-data group) do
	 (print edition stream))))

(defparameter *g* (make-instance 'group))

(defun group-save (group)
  (with-open-file (out (make-pathname :directory  "/home/stack/Documents/lisp/subtext"
				      :name (group-name group) 
				      :type "bnn")
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :supersede
		       )
    (group-serialize out group))
  nil)


