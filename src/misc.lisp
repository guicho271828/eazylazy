(in-package :eazylazy)
(use-syntax :annot)


@export
(defmacro llist (a &rest args)
  "list using lcons"
  `(lcons ,a ,(when args `(llist ,@args))))

@export
(defmacro ltree (tree)
  (if (consp tree)
      `(llist ,@(mapcar (lambda (e) `(ltree ,e)) tree))
      tree))


@export
(defmacro fpop (place)
  (multiple-value-bind (vars vals store-vars writer reader)
      (get-setf-expansion place)
    @ignorable reader writer
    (with-gensyms (list-head car)
      `(let* (,@(mapcar #'list vars vals)
              (,list-head ,reader))
         (when ,list-head
           (let* ((,car (fcar ,list-head))
                  (,(car store-vars) (fcdr ,list-head))
                  ,@(cdr store-vars))
             ,writer
             ,car))))))

@export
(defmacro lpush (obj place)
  `(push (lambda () ,obj) ,place))

;; (fpop (aref a i j))
;; (fpop (first (progn (incf i) a)))


@export
(defmacro llet (bindings &body body)
  `(let (,@(loop for (var value) in bindings
              collect `(,var (lambda () ,value))))
     ,@body))
