(defpackage :range
  (:use #:cl)
  (:shadow cl:find cl:delete)
  (:export
   :range :width :data :child :dad :l :make-range
   :ranges :root :ht
   :root
   :new
   :end
   :bounds
   :widen :narrow
   :at
   :find
   :kids
))
