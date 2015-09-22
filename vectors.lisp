(defpackage #:clg-vec
  (:use #:cl #:clg-util)
  (:export #:mapvs
           #:mapvec
           #:mapv

           #:vector-of
           #:subvector

           #:setv
           #:v+
           #:incv
           #:v-
           #:decv
           #:v*
           #:mulv
           #:v/
           #:divv

           #:absvec
           #:unitvec
           #:distance
           #:scalar
           #:unit-scalar
           #:angle
           #:cross3

           #:orthogonal-factor
           #:orthogonal-vector
           #:line-vector
           #:line-distance
           ))

(in-package #:clg-vec)


(flet ((map-line (function line &rest vectors)
         (declare (type function function)
                  (type integer line))
         (apply function (mapcar (lambda (vector) (aref vector line)) vectors))))
  (defun mapvs (size function vec &rest vecs)
    (dotimes (i size)
      (setf (aref vec i) (apply #'map-line function i vecs)))
    vec))

(defun mapvec (function vec &rest vecs)
  (let* ((size (array-dimension vec 0))
         (vector (make-array size)))
    (apply #'mapvs size function vector vec vecs)))

(defun mapv (function vec &rest vecs)
  (apply #'mapvs (array-dimension vec 0) function vec vec vecs))


(defun vector-of (type &rest elements)
  (make-array (length elements) :element-type type :initial-contents elements))

(defun subvector (vector &optional (start 0) end &aux (len  (- (if end end (array-total-size vector)) start)))
  (make-array len :element-type (array-element-type vector)
              :displaced-to vector :displaced-index-offset start))

;;;mathematical

(defun setv (veca vecd)
  (mapv (lambda (a d) (declare (ignore a)) d) veca vecd))

(defun v+ (&rest vectors)
  (apply #'mapvec #'+ vectors))
(defun incv (&rest vectors)
  (apply #'mapv #'+ vectors))

(defun v- (&rest vectors)
  (apply #'mapvec #'- vectors))
(defun decv (&rest vectors)
  (apply #'mapv #'- vectors))

(defmacro vector-function (fo fi var)
  `(lambda (num) (apply ,fo num (if ,var (list (apply ,fi ,var))))))

(defun v* (vector &rest numbers)
  (mapvec (vector-function #'* #'* numbers) vector))
(defun mulv (vector &rest numbers)
  (mapv (vector-function #'* #'* numbers) vector))

(defun v/ (vector &rest numbers)
  (mapvec (vector-function #'/ #'* numbers) vector))
(defun divv (vector &rest numbers)
  (mapv (vector-function #'/ #'* numbers) vector))

(defun absvec (vector)
  (sqrt (reduce #'+ (mapvec (lambda (line) (expt line 2)) vector))))

(defun unitvec (vector)
  (let ((abs (absvec vector)))
    (if (zerop abs)
        (v* vector 0)
        (v/ vector (absvec vector)))))

(defun distance (vectora vectord)
  (absvec (v- vectora vectord)))

(defun scalar (vectora vectord)
  (reduce #'+ (map '(vector number) #'* vectora vectord)))

(defun unit-scalar (veca vecd)
  (/ (scalar veca vecd) (absvec veca) (absvec vecd)))

(defun angle (veca vecd)
  (mod (acos (unit-scalar veca vecd)) pi))

(defun cross3 (veca vecd)
  (declare (type (vector number 3) veca vecd))
  (vector-bind (a1 a2 a3) veca
    (vector-bind (d1 d2 d3) vecd
      (vector
       (- (* a2 d3) (* a3 d2))
       (- (* a3 d1) (* a1 d3))
       (- (* a1 d2) (* a2 d1))))))


#+nil
(defun test-angle (angle vector)
  (angle vector
         (vector (+ (* (aref vector 0) (cos angle))
                    (* (aref vector 1) (sin angle)))
                 (- (* (aref vector 1) (cos angle))
                    (* (aref vector 0) (sin angle))))))
                 

(defun orthogonal-factor (point line)
  (/ (scalar point line) (scalar line line)))

(defun orthogonal-vector (point line)
  (v* line (orthogonal-factor point line)))

(defun line-vector (point line)
  (v- (orthogonal-vector point line) point))

(defun line-distance (point line)
  (absvec (line-vector point line)))


