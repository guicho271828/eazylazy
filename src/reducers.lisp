(in-package :eazylazy)
(use-syntax :annot)

@export
(defun fsome (pred first-seq &rest more-seqs)
  (typecase first-seq
    (cons   (%fsome-cons   pred   first-seq more-seqs))
    (vector (%fsome-vector pred 0 first-seq more-seqs))))

(defun %fsome-cons (pred first-seq more-seqs)
  (or (apply       pred (fcar first-seq) (mapcar #'fcar more-seqs))
      (if (and (fcdr first-seq)
               (every #'fcdr more-seqs))
          (%fsome-cons pred (fcdr first-seq) (mapcar #'fcdr more-seqs))
          nil)))

(defun %fsome-vector (pred n first-seq more-seqs)
  (or (apply pred
             (force (handler-bind ((type-error
                                    (lambda (c)
                                      (declare (ignore c))
                                      (return-from %fsome-vector nil))))
                      (aref first-seq n)))
             (mapcar (lambda (a)
                       (force
                        (handler-bind ((type-error
                                        (lambda (c)
                                          (declare (ignore c))
                                          (return-from %fsome-vector nil))))
                          (aref a n))))
                     more-seqs))
      (%fsome-vector pred (1+ n) first-seq more-seqs)))

@export
(defun fevery (pred first-seq &rest more-seqs)
  (typecase first-seq
    (cons   (%fevery-cons   pred   first-seq more-seqs))
    (vector (%fevery-vector pred 0 first-seq more-seqs))))

(defun %fevery-cons (pred first-seq more-seqs)
  (and (apply pred (fcar first-seq) (mapcar #'fcar more-seqs))
       (if (and (fcdr first-seq)
                (every #'fcdr more-seqs))
           (%fevery-cons pred (fcdr first-seq) (mapcar #'fcdr more-seqs))
           t)))

(defun %fevery-vector (pred n first-seq more-seqs)
  (and 
   (apply pred
          (force (handler-bind ((type-error
                                 (lambda (c)
                                   (declare (ignore c))
                                   (return-from %fevery-vector t))))
                   (aref first-seq n)))
          (mapcar (lambda (a)
                    (force
                     (handler-bind ((type-error
                                     (lambda (c)
                                       (declare (ignore c))
                                       (return-from %fevery-vector t))))
                       (aref a n))))
                  more-seqs))
   (%fevery-vector pred (1+ n) first-seq more-seqs)))

