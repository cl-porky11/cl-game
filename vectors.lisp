(defpackage #:clg-vec
  (:use #:cl)
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
           #:distance
           #:scalar
           #:angle
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

(defun subvector (vector &optional (start 0) end &aux (len (1+ (- (if end end (length vector)) start))))
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
  `(lambda (num) (funcall ,fo (apply ,fi num ,var))))

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

(defun distance (vectora vectord)
  (absvec (v- vectora vectord)))

(defun scalar (vectora vectord)
  (reduce #'+ (map '(vector number) #'* vectora vectord)))

(defun angle (veca vecd)
  (mod (acos (/ (scalar veca vecd) (absvec veca) (absvec vecd))) pi))

#+nil
(defun test-angle (angle vector)
  (angle vector
         (vector (+ (* (aref vector 0) (cos angle))
                    (* (aref vector 1) (sin angle)))
                 (- (* (aref vector 1) (cos angle))
                    (* (aref vector 0) (sin angle))))))
                 

(defun orthogonal-length (start line point)
  (/ (scalar (v- point start) line) (scalar line line)))

(defun orthogonal-projection (start line point)
  (v+ start (v* line (orthogonal-length start line point))))

(defun line-vector (start line point)
  (v- (orthogonal-projection start line point) point))

(defun line-distance (start line point)
  (absvec (line-vector start line point)))


