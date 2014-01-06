(in-package :eazylazy)
(use-syntax :annot)

@export
(defmacro forcef (place)
  "Destructive version of `force'.
It forces the value of the place and destructively set the value
with the returned value.
Use only for the reader-intensive use."
  (multiple-value-bind (vars vals store-vars writer reader)
      (get-setf-expansion place)
    @ignorable reader
    `(let* (,@(mapcar (lambda (var val)
                        (list var val))
                      vars vals)
            (,(car store-vars) (force ,reader)))
       ,writer)))

@export
(defmacro lcons (a b)
  "cons whose car and cdr is lazy"
  `(cons (delay ,a)
         (delay ,b)))

(defmacro define-forced (name accessor &rest args)
  `(defun ,name ,args
     (force (,accessor ,@args))))

(defmacro define-forced! (name accessor &rest args)
  `(defun ,name ,args
     (forcef (,accessor ,@args))))

(defmacro define-forced-many (&rest args-list)
  `(progn
     ,@(mapcar (lambda (args)
                 (let* ((forced (symbolicate "F" (car args)))
                        (forced! (symbolicate forced "!")))
                   `(progn (export ',forced)
                           (export ',forced!)
                           (define-forced ,forced ,@args)
                           (define-forced! ,forced! ,@args))))
               args-list)))

(define-forced-many
  (cdr lcons)
  (car lcons)
  (cddr lcons)
  (cdddr lcons)
  (cddddr lcons)
  (caar lcons)
  (caaar lcons)
  (caaaar lcons)
  (cadr lcons)
  (cdar lcons)
  (first lcons)
  (second lcons)
  (third lcons)
  (fourth lcons)
  (fifth lcons)
  ;; lazy array
  (aref array &rest subscripts))

