#|
  This file is a part of eazylazy project.
  Copyright (c) 2014 Masataro Asai
|#


(in-package :cl-user)
(defpackage eazylazy.test-asd
  (:use :cl :asdf))
(in-package :eazylazy.test-asd)


(defsystem eazylazy.test
  :author "Masataro Asai"
  :license ""
  :depends-on (:eazylazy
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) (PROGN (EVAL (READ-FROM-STRING "(fiveam:run! :eazylazy)")) (CLEAR-SYSTEM C))))
