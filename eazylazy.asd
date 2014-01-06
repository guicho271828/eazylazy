#|
  This file is a part of eazylazy project.
  Copyright (c) 2014 Masataro Asai
|#

#|
  Author: Masataro Asai
|#



(in-package :cl-user)
(defpackage eazylazy-asd
  (:use :cl :asdf))
(in-package :eazylazy-asd)


(defsystem eazylazy
  :version "0.1"
  :author "Masataro Asai"
  :mailto ""
  :license ""
  :depends-on (:trivial-lazy
               :cl-syntax-annot)
  :components ((:module "src"
                :components
                ((:file :package)
                 (:file :basics)
                 (:file :mappers)
                 (:file :misc))
                :serial t))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"includes/README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  
  :in-order-to ((test-op (load-op eazylazy.test))))
