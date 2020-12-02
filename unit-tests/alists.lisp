(in-package #:metabang-bind-test)

(deftestsuite test-alists (metabang-bind-test)
  ())

(addtest (test-alists)
  basic-access
  (ensure-same
   (bind (((:alist a (b _) (c _ 2) (dd d))
           `((b . #\b) (a . #\a) (d . #\d))))
     (list a b c dd))
   '(#\a #\b 2 #\d) :test 'equalp))
