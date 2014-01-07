
(in-package :eazylazy)
(use-syntax :annot)
(eval-when (:compile-toplevel :load-toplevel :execute)
  @export
  @inline
  (defun lazy-consp (x)
    (consp x))
  @export
  @inline
  (defun lazy-conscar (x)
    (fcar x))
  @export
  @inline
  (defun lazy-conscdr (x)
    (fcdr x))
  (optima:defpattern lcons (lcar lcdr)
    `(lazy-cons (car ,lcar) (cdr ,lcdr))))