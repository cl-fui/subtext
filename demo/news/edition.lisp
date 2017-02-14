(in-package :subtext)

;;; edition

(defstruct edition name parts (bytes 0) poster)
 

(defun create-edition (name size poster)
  (make-edition
   :name name
   :parts (make-array size :element-type 'fixnum :initial-element 0)
   :poster poster))

(defun edition-count-parts (edition)
  "number of parts here"
  (count-if-not #'zerop (edition-parts edition)))

(defun edition-max (edition)
  (reduce #'max (edition-parts edition)))

(defun edition-full? (edition)
  (let ((parts (edition-count-parts edition))
	(total (length (edition-parts edition))))
    (or (= parts total); either all parts present
	(and (> total 1) ;or >1 total and all parts but 0 present
	     (not (zerop (reduce #'min (edition-parts edition) :start 1)))))))
 
;;------------------------------------------------------------------------------




