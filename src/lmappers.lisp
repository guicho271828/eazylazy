(in-package :eazylazy)
(use-syntax :annot)

;; lazy mappers

;; copied from my own repo of trivial-lazy
;; https://github.com/guicho271828/trivial-lazy/tree/at-least-provide-ftype-to-memo
(deftype thunk (&optional result) `(function () ,result))
(declaim (ftype (function ((thunk) &key (:thread-safe boolean)) (values (thunk))) memo))
(declaim (inline memo))
(declaim (inline force))

@export
(defun lmapcar (function &rest llists)
  "Apply FUNCTION to successive elements of lazy lists. Return lazy
   list of FUNCTION return values."

  (labels ((next (fn llists)
           (lcons (apply fn (mapcar #'fcar llists))
                  (when (every #'fcdr llists)
                    (next fn (mapcar #'fcdr llists))))))
    ;;
    (declare (ftype (function ((function) (cons))
                              (cons (thunk) (thunk (cons))))
                    next))
    (next (coerce function 'function) llists)))

@export
(defun lmapc (function &rest llists)
  "Apply FUNCTION to successive elements of lazy lists.
Return (thunk (or (thunk) null))."
  ;;
  (labels ((next (fn llists)
             (delay (progn
                      (apply fn (mapcar #'fcar llists))
                      (when (every #'fcdr llists)
                        (next fn (mapcar #'fcdr llists)))))))
    ;;
    (declare (ftype (function ((function) (cons)) (thunk (or (thunk) null))) next))
    (next (coerce function 'function) llists)))

@export
(defun lmaplist (function &rest llists)
  "Apply FUNCTION to successive elements of lazy lists. Return lazy
   list of FUNCTION return values."
  (labels ((next (fn llists)
           (lcons (apply fn llists)
                  (when (every #'fcdr llists)
                    (next fn (mapcar #'fcdr llists))))))
    ;;
    (declare (ftype (function ((function) (cons))
                              (cons (thunk) (thunk (cons))))
                    next))
    (next (coerce function 'function) llists)))

@export
(defun lmapl (function &rest llists)
  "Apply FUNCTION to successive elements of lazy lists. Return the second argument."
  ;;
  (labels ((next (fn llists)
             (delay (progn
                      (apply fn llists)
                      (when (every #'fcdr llists)
                        (next fn (mapcar #'fcdr llists)))))))
    ;;
    (declare (ftype (function ((function) (cons)) (thunk (or (thunk) null))) next))
    (next (coerce function 'function) llists)))
