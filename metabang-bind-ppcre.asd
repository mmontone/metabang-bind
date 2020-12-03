(defsystem metabang-bind-ppcre
  :version "0.8.0"
  :author "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License"
  :description "Bind is a macro that generalizes multiple-value-bind, let, let*, destructuring-bind, structure and slot accessors, and a whole lot more."
  :components ((:module
                "dev"
                :serial t
                :components
                ((:file "bind-cl-ppcre"))))
  :depends-on (:metabang-bind :cl-ppcre))
