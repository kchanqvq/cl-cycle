(in-package #:cl-cycle)

(defun scale (note scale)
  (m (lambda (note scale)
       (bind (((:values oct n) (floor note (length scale))))
         (* (nth n scale) (expt 2 oct))))
     note scale))

(defun m+ (&rest args)
  (apply #'m #'+ args))

(defun m- (&rest args)
  (apply #'m #'- args))

(defun mlist (&rest args)
  (apply #'m #'list args))

(sera:def a-maj (iter (for i in '(0 2 4 5 7 9 11))
                  (collect (* 440 (expt 2 (/ i 12))))))
