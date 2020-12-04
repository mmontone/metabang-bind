(in-package :metabang.bind)

;; trivial implementation of binding lambda
;; TODO: improve (parse lambda-list)

(defmacro blambda (args &body body)
  "Binding lambda"
  (let ((bindings (loop for arg in args
                        collect (cons (gensym) arg))))
    `(lambda ,(mapcar 'first bindings)
       (bind:bind ,(loop for (binding . spec) in bindings
                         collect `(,spec ,binding))
         ,@body))))

#+(or)
(funcall (blambda ((_ . y))
           (print y)) (cons 1  2))

#+(or)
(funcall (blambda ((x . _))
           (print x)) (cons 1  2))
