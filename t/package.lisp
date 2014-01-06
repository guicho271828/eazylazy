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

(test lcons
  (let ((lc (lcons 1 2)))
    (is (= 1 (fcar lc)))
    (is (typep (car lc) 'function))
    (is (= 1 (fcar lc)))
    (is (= 1 (fcar! lc)))
    (is (typep (car lc) 'number))
    
    (is (= 2 (fcdr lc)))
    (is (typep (cdr lc) 'function))
    (is (= 2 (fcdr lc)))
    (is (= 2 (fcdr! lc)))
    (is (typep (cdr lc) 'number))))

(test (lcons-rec :depends-on lcons)
  (labels ((rec (n)
             (lcons n (rec (1+ n)))))
    (let ((lstream (rec 0)))
      ;; you can either write in imperative/destructive style
      (is (= 0 (fpop lstream)))
      (is (= 1 (fpop lstream)))
      (is (= 2 (fpop lstream)))
      (is (= 3 (fpop lstream)))
      (is (= 4 (fpop lstream))))

    (let ((lstream (rec 0))
          (max 5))
      ;; or declarative/functional style
      (labels ((rec2 (expected lcons)
                 (when (< expected max)
                   (is (= expected (fcar lcons)))
                   (rec2 (1+ expected) (fcdr lcons)))))
        (rec2 0 lstream)))))

