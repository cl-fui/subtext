(defpackage :range
  (:use #:cl)
  (:shadow cl:find cl:delete)
  (:export
   :range :width :child :dad :l :make
   :new-in
   :end
   :bounds
   :widen :narrow
   :at
   :kids
   :subrange :sub-of
))
