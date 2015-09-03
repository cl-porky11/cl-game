

(flet ((inc (fn power limit args)
         (* (expt (the (real 0)
                       (apply fn
                              (mapcar (lambda (summand) (expt (/ (the (real 0) summand) limit) power))
                                      args)))
                  (/ power))
            limit)))
  (defun up (power limit &rest args)
    (inc #'+ power limit args))
  (defun down (power limit &rest args)
    (inc #'- power limit args)))

(flet ((inc (fn power limit arg args)
         (* (expt (the (real 0)
                       (apply fn
                              (expt (/ (the (real 0) arg) limit) power)
                              (mapcar (lambda (summand) (/ (the (real 0) summand) limit))
                                      args)))
                  (/ power))
            limit)))
  (defun up (power limit arg &rest args)
    (inc #'+ power limit arg args))
  (defun down (power limit arg &rest args)
    (inc #'- power limit arg args)))




(define-modify-macro upf (&optional (power 2) (limit 1) (arg 1) &rest args)
  (lambda (arg power limit &rest args) (apply #'up power limit arg args)))

(define-modify-macro downf (&optional (power 1/2) (limit 1) (arg 1) &rest args)
  (lambda (arg power limit &rest args) (apply #'down power limit arg args)))

(defun test-down (num) (- num (down 1/2 8 num 4)))
