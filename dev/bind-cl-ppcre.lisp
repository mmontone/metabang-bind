(in-package #:metabang.bind.developer)

#+wrong
(defmethod bind-generate-bindings ((kind (eql :re)) variable-form value-form)
  ;; (:re "re" vars)
  (bind (((regex &rest vars) variable-form)
         (gok (gensym "ok"))
         (gblock (gensym "block"))
         ((:values vars ignores) (bind-fix-nils vars)))
    `((let ((,gok nil))
        (block ,gblock
          (flet ((doit (,@vars)
                   ,@(when ignores `((declare (ignore ,@ignores))))
                   (return-from ,gblock
                     (progn ,@(bind-macro-helper
                               remaining-bindings declarations body)))))
            (cl-ppcre:register-groups-bind
                ,vars (,regex ,(first value-form) :sharedp t)
              ,(bind-filter-declarations
                declarations variable-form)
              (setf ,gok t)
              (doit ,@vars))
            (unless ,gok
              (doit ,@(make-list (length vars) :initial-element nil)))))))))

;; simple but doesn't execute inner code if no bindings found
;; which isn't very bind-like
#+(or)
(defmethod bind-generate-bindings ((kind (eql :regex)) variable-form value-form)
  ;; (:re "re" vars)
  (bind (((regex &rest vars) variable-form))
    `(cl-ppcre:register-groups-bind ,vars (,regex ,(first value-form) :sharedp t))))

#+(or)
;; doesn't handle ignores
(defmethod bind-generate-bindings
    ((kind (eql :re)) variable-form value-form
     body declarations remaining-bindings)
  ;; (:re "re" vars)
  (bind (((regex &rest vars) variable-form)
         (gok (gensym "ok"))
         (gblock (gensym "block")))
    `((let ((,gok nil))
        (block ,gblock
          (flet ((doit (,@vars)
                   (return-from ,gblock
                     ,@(bind-macro-helper
                        remaining-bindings declarations body))))
            (cl-ppcre:register-groups-bind
                ,vars (,regex ,(first value-form) :sharedp t)
              ,(bind-filter-declarations
                declarations variable-form)
              (setf ,gok t)
              (doit ,@vars))
            (unless ,gok
              (doit ,@(make-list (length vars) :initial-element nil)))))))))

;; A rewrite of ppcre:register-groups-bind, that runs body regardless of if there are matches or not.
(defmacro register-groups-bind (var-list (regex target-string
                                                &key start end sharedp)
                                &body body)
  "Executes BODY with the variables in VAR-LIST bound to the
corresponding register groups after TARGET-STRING has been matched
against REGEX, i.e. each variable is either bound to a string or to
NIL.  For each element
of VAR-LIST which is NIL there's no binding to the corresponding
register group.  The number of variables in VAR-LIST must not be
greater than the number of register groups.  If SHAREDP is true, the
substrings may share structure with TARGET-STRING."
  (ppcre::with-rebinding (target-string)
    (ppcre::with-unique-names (match-start match-end reg-starts reg-ends
                                    start-index substr-fn)
      (let ((var-bindings
              (loop for (function var) in (ppcre::normalize-var-list var-list)
                    for counter from 0
                    when var
                      collect `(,var (when ,match-start
                                       (let ((,start-index
                                             (aref ,reg-starts ,counter)))
                                       (if ,start-index
                                           (funcall ,function
                                                    (funcall ,substr-fn
                                                             ,target-string
                                                             ,start-index
                                                             (aref ,reg-ends ,counter)))
                                           nil)))))))
        `(multiple-value-bind (,match-start ,match-end ,reg-starts ,reg-ends)
             (ppcre:scan ,regex ,target-string :start (or ,start 0)
                                               :end (or ,end (length ,target-string)))
           (declare (ignore ,match-start ,match-end))
           ,@(unless var-bindings
               `((declare (ignore ,reg-ends))))
           ,@(if var-bindings
                 `((let* ,(list*
                           `(,substr-fn (if ,sharedp #'ppcre::nsubseq #'subseq))
                           var-bindings)
                     ,@body))
                 body))))))

;; simple but doesn't execute inner code if no bindings found
;; which isn't very bind-like
(bind::defbinding-form ((:regex :re) :use-values-p nil)
  `(register-groups-bind ,(rest bind::variables)
       (,(first bind::variables)
        ,bind::values :sharedp t)))

#+(or)
(bind (((:regex "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
             fname lname date month year) "Frank Zappa 21.12.1940"))
  (list fname lname date month year))

#+(or)
(macroexpand-1
 '(bind (((:regex "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
           fname lname date month year) "Frank Zappa 21.12.1940"))
   (list fname lname date month year)))

#+(or)
(bind (((:regex "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
             fname lname nil month year) "Frank Zappa 21.12.1940"))
  (list fname lname month year))

#+(or)
(bind (((:regex "(a|b)+" first) "cccc"))
  (format nil "This should still be printed: ~A" first))
