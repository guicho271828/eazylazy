(in-package :eazylazy)
(use-syntax :annot)

;; picked from sbcl source

@inline
(defun fmap1! (fun-designator original-arglists accumulate take-car)
  (let ((fun (coerce fun-designator 'function)))
    (let* ((arglists (copy-list original-arglists))
           (ret-list (list nil))
           (temp ret-list))
      (do ((res nil)
           (args '() '()))
          ((dolist (x arglists nil)
             (when (null (forcef x))
               (return t)))
           (if accumulate
               (cdr ret-list)
               (car original-arglists)))
        ;; ----  per-iteration ----
        ;; build arguments list in args
        (do ((l arglists (cdr l))) ; arglist = ( (arg1-1 arg1-2 ...) (arg2-1 arg2-2 ...) ...)
            ((null l))
          (push (if take-car (fcar! (car l)) (car l)) args)
          (forcef (car (car l)))
          (setf (car l) (fcdar! l)))
        ;; call the function
        (setq res (apply fun (nreverse args))) ; res = result
        ;; accumulate the result into temp
        (case accumulate
          (:nconc (setq temp (last (nconc temp res))))
          (:list (rplacd temp (list res))
                 (setq temp (cdr temp))))))))

@export
(defun fmapc (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists. Return the second argument."
  (fmap1! function (cons list more-lists) nil t))

@export
(defun fmapcar (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return list of FUNCTION
   return values."
  (fmap1! function (cons list more-lists) :list t))
@export
(defun fmapcan (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return NCONC of FUNCTION
   results."
  (fmap1! function (cons list more-lists) :nconc t))
@export
(defun fmapl (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return NIL."
  (fmap1! function (cons list more-lists) nil nil))
@export
(defun fmaplist (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return list of results."
  (fmap1! function (cons list more-lists) :list nil))

;; @export
;; (defun fmapcon (function list &rest more-lists)
;;   "Apply FUNCTION to successive CDRs of lists. Return NCONC of results."
;;   (fmap1! function (cons list more-lists) :nconc nil))

@inline
(defun fmap1 (fun-designator original-arglists accumulate take-car)
  (let ((fun (coerce fun-designator 'function)))
    (let* ((arglists (copy-list original-arglists))
           (ret-list (list nil))
           (temp ret-list))
      (do ((res nil)
           (args '() '()))
          ((dolist (x arglists nil)
             (when (null (force x))
               (return t)))
           (if accumulate
               (cdr ret-list)
               (car original-arglists)))
        ;; ----  per-iteration ----
        ;; build arguments list in args
        (do ((l arglists (cdr l))) ; arglist = ( (arg1-1 arg1-2 ...) (arg2-1 arg2-2 ...) ...)
            ((null l))
          (push (if take-car (fcar (car l)) (car l)) args)
          (force (car (car l)))
          (setf (car l) (fcdar l)))
        ;; call the function
        (setq res (apply fun (nreverse args))) ; res = result
        ;; accumulate the result into temp
        (case accumulate
          (:nconc (setq temp (last (nconc temp res))))
          (:list (rplacd temp (list res))
                 (setq temp (cdr temp))))))))

@export
(defun fmapc (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists. Return the second argument."
  (fmap1 function (cons list more-lists) nil t))

@export
(defun fmapcar (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return list of FUNCTION
   return values."
  (fmap1 function (cons list more-lists) :list t))
@export
(defun fmapcan (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return NCONC of FUNCTION
   results."
  (fmap1 function (cons list more-lists) :nconc t))
@export
(defun fmapl (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return NIL."
  (fmap1 function (cons list more-lists) nil nil))
@export
(defun fmaplist (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return list of results."
  (fmap1 function (cons list more-lists) :list nil))

;; @export
;; (defun fmapcon (function list &rest more-lists)
;;   "Apply FUNCTION to successive CDRs of lists. Return NCONC of results."
;;   (fmap1 function (cons list more-lists) :nconc nil))

