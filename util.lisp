(in-package #:cl-cycle)

(defun scale (note scale)
  (m (lambda (note scale)
       (bind (((:values oct n) (floor note (length scale))))
         (* (nth n scale) (expt 2 oct))))
     note scale))

(defun quantize (note scale)
  (m (lambda (note scale)
       (bind (((:values oct n) (floor note 12)))
         (+ (* oct 12) (iter (for m in scale)
                         (finding m minimizing (abs (- m n)))))))
     note scale))

(defun m+ (&rest args)
  (apply #'m #'+ args))

(defun m- (&rest args)
  (apply #'m #'- args))

(defun mlist (&rest args)
  (apply #'m #'list args))

(sera:def 12tet
  (iter (for i below 12)
    (collect (* 440 (expt 2 (/ i 12))))))
