(in-package #:metabang-bind-test)

(deftestsuite test-plists (metabang-bind-test)
  ())

(addtest (test-plists)
  basic-access
  (ensure-same
   (bind (((:plist a (b _) (c _ 2) (dd d)) '(:b #\b :a #\a :d #\d)))
     (list a b c dd))
   '(#\a #\b 2 #\d) :test 'equalp))

(addtest (test-plists)
  rest-args
  (ensure-same
   (bind (((:plist x &rest xs) (list :x 22 :y 44)))
     (list* :x x xs))
   (list :x 22 :y 44)))

(addtest (test-plists)
  plist-
  (ensure-same
   (bind (((:plist- a (b _) (c _ 2) (dd d)) '(b "B" a "A" d "D")))
     (list a b c dd))
   (list "A" "B" 2 "D")))
