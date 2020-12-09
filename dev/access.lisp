(in-package :metabang-bind)

(defbinding-form (:access)
  `(access:with-access ,variables ,values))

#+(or)
(bind (((:access x) (list :x 22)))
  x)

;; This doesn't work:
#+(or)
(let ((data (list :x 22)))
  (bind (((:access x) data))
    (setf x 44)
    data))

;; This works:
#+(or)
(let ((data (list (cons 'x 22))))
  (bind (((:access x) data))
    (setf x 44)
    data))

(defbinding-form (:access-values)
  `(access:with-access-values ,variables ,values))

#+(or)
(bind (((:access-values x) (list :x 22)))
  x)

;; No setf for access-values:
#+(or)
(let ((data (list :x 22)))
  (bind (((:access-values x) data))
    (setf x 44)
    data))

;; No setf for access-values:
#+(or)
(let ((data (list (cons 'x 22))))
  (bind (((:access-values x) data))
    (setf x 44)
    data))
