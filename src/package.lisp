#|
  This file is a part of eazylazy project.
  Copyright (c) 2014 Masataro Asai
|#

(in-package :cl-user)
(defpackage eazylazy
  (:use :cl :trivial-lazy :cl-syntax)
  (:import-from :alexandria :symbolicate :with-gensyms :rcurry))
(in-package :eazylazy)
(use-syntax :annot)
;; blah blah blah.

