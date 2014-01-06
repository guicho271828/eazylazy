#|
  This file is a part of eazylazy project.
  Copyright (c) 2014 Masataro Asai
|#

(in-package :cl-user)
(defpackage :eazylazy.test
  (:use :cl
        :eazylazy
        :fiveam))
(in-package :eazylazy.test)



(def-suite :eazylazy)
(in-suite :eazylazy)

;; run test with (run! test-name) 
;;   test as you like ...

(test eazylazy

  )

