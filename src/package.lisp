#|
  This file is a part of eazylazy project.
  Copyright (c) 2014 Masataro Asai
|#

(in-package :cl-user)
(defpackage eazylazy
  (:use :cl :trivial-lazy :cl-syntax :alexandria)
  (:export :delay :force))
(in-package :eazylazy)
(use-syntax :annot)
;; blah blah blah.

