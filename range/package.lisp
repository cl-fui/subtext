(defpackage :range
  (:use #:cl)
  (:shadow cl:find cl:delete)
  (:export
   :range :width :child :dad :l :make
   :new-in
   :end
   :bounds
   :widen :narrow
   :at :actual
   :kids
   :sub
   :widen-baby
   :childest
))
