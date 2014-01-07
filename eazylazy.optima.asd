#|
  This file is a part of eazylazy project.
  Copyright (c) 2014 Masataro Asai
|#

#|
  Author: Masataro Asai
|#


(defsystem eazylazy.optima
  :version "0.1"
  :author "Masataro Asai"
  :mailto ""
  :license ""
  :depends-on (:trivial-lazy
               :cl-syntax-annot
               :eazylazy)
  :components ((:module "src"
                :components
                ((:file :optima-pattern))
                :serial t))
  :description "provides a pattern for optima")
