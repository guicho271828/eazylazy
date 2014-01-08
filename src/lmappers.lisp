(in-package :eazylazy)
(use-syntax :annot)

;; lazy mappers

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
