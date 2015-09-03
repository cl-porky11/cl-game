(defpackage #:clg-crypt
  (:use #:cl #:alexandria #:cl-prime-maker #:util)
  (:export #:generate-keypair))

(in-package :clg-crypt)

(defgeneric generate-keypair (type &rest parameters))

(defgeneric crypt (en type key value))

(defgeneric number->string (type number))

(defgeneric string->number (type key))

(defmethod number->string ((type (eql :chars)) number))

(declaim (ftype (function (non-negative-integer &optional positive-integer list) list)
                non-negative-integer->list))

(defun non-negative-integer->list (number &optional (base 2) list)
  (multiple-value-bind (div rest) (floor (abs number) base)
    (let ((list (cons rest list)))
      (if (zerop div)
          list
          (number->list div base list)))))

(defun list->non-negative-integer (list &optional (base 2)
                                   &aux (number 0))
  (dolist (n list)
    (mulf number base)
    (incf number n))
  number)

(defun non-negative-integer->string (number &optional (base 256) (code-char #'code-char))
  (map 'string code-char (non-negative-integer->list number base)))

(defun string->non-negative-integer (string &optional (base 256) (char-code #'char-code))
  (list->non-negative-integer (map 'list char-code string) base))

  

(defmethod generate-keypair ((type (eql :rsa)) &rest parameters)
  (destructuring-bind (p q e) parameters
    (assert (= (gcd p q) 1))
    (let* ((n (* p q))
           (phi (* (1- p) (1- q))))
      (assert (not (and (< 1 e phi) (integerp (/ phi e)))))
      (let ((d (modular-inverse e phi)))
        (values (list e n)
                (list d n))))))

(defmethod generate-keypair ((type (eql :dsa)) &rest parameters)
  (destructuring-bind (p g a) parameters
    (assert (not (zerop (nth-value 1 (floor p g)))))
    (values a
            (mod (expt g a) p))))

;(defmethod generate-keypair ((type (eql :mhk)) &rest parameters)
;  (destructuring-bind (


(defun rsa-crypt (key value)
  (mod (expt value (car key)) (cadr key)))

#+nil
(defmethod crypt (en (type (eql :rsa)) (key 'cons) value)
  (rsa-crypt (key value)))

;(defun encrypt-dsa
;
;    (gcd 11 13)
 
#|

(let ((counter 1)
      (prime-numbers nil))
  (defun prime-numbers (&optional (number 1))
    (loop while (< counter number)
       do (incf counter)
       do (dolist (prime-number prime-numbers (push counter prime-numbers))
            (when (/= (gcd prime-number counter) 1)
              (return))))
    (reverse prime-numbers)))

(defun egcd (a b)
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r)))
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s)))
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u)))
       (q nil))
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))
    (setq q (floor (cdr r) (car r)))))

#|(defun modular-inverse (a m)
  (loop for i from 0
     if (= (mod (* a i) m) 1)
       return i))
|#


(defun modular-inverse (a m)
  (multiple-value-bind (r s) (egcd a m)
    (assert (= 1 r))
    (mod s m)))

(defun is-prime (number &aux (root (isqrt number)))
  (dolist (prime (prime-numbers root) t)
    (when (< root prime) (return t))
    (and (integerp (/ number prime))
         (return))))

(defun hash (seed) seed)

(defun expt-mod (base power divisor)
  (if (< 0 power)
    (mod (* base (expt-mod base (1- power) divisor)) divisor)
    1))

(labels
    ((fun (base power divisor num)
       (if (< 0 power)
           (fun base (1- power) divisor (mod (* base num) divisor))
           num)))
  (defun expt-mod (base power divisor)
    (fun base power divisor 1)))


(defun expt-mod (base power divisor)
  (mod (expt base power) divisor))

(defun generate-prime-number (length seed)
  (cond
    ((< length 2))
    ((< length 33)
     (do* ((prime-seed seed (+ prime-seed 2)))
          (nil)
       (let* ((expt (expt 2 (1- length)))
              (number
               (1+ (* 2 (floor (+ expt (mod (+ (hash prime-seed) (hash (1+ prime-seed))) expt)) 2)))))
         (if (is-prime number)
           (return (values number prime-seed))))))
    (t (multiple-value-bind (number prime-seed) (generate-prime-number (1+ (ceiling length 2)) seed)
         (when number
           (let* ((x 0)
                  (outlen (log number 2))
                  (iterations (ceiling length outlen)))
             (dotimes (i iterations)
               (incf x (* (hash (+ prime-seed i)) (expt 2 (* i outlen)))))
             (incf prime-seed iterations)
             (let ((te (ceiling x (* 2 number))))
               (loop
                  (when (< (expt 2 length) (1+ (* 2 te number)))
                       (setq te (ceiling (expt 2 (1- length)) (* 2 number))))
                  (let ((c (1+ (* 2 te number)))
                        (a 0))
                    (dotimes (i iterations)
                      (incf a (round (* (hash (+ prime-seed i)) (expt 2 (* i outlen))))))
                    (incf prime-seed iterations)
                    (setq a (+ 2 (mod a (- c 3))))
                    (let ((z (expt-mod a (* 2 te) c)))
                      (and (= (gcd (1- z) c) 1)
                           (= (expt-mod z number c) 1)
                           (return (values c prime-seed)))))
                  (incf te)))))))))






|#
