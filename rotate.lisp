(defpackage #:clg-rot
  (:use #:cl #:clg-vec #:alexandria)
  (:export #:make-rotation
           #:rotation-angle
           #:rotation-axis
           #:r*
           #:mulr
           #:rotate-vector
           #:rotation-to-vector
           #:vector-rotation
           #:rotation-vector
           ))

(in-package #:clg-rot)

(declaim (inline make-rotation
                 rotation-angle rotation-axis rotation-axis-
                 (setf rotation-angle) (setf rotation-axis) (setf rotation-axis-)))

(defun make-rotation (&key angle axis)
  (v* (unitvec axis) angle))

(defun rotation-angle (rot)
  (absvec rot))

(defun (setf rotation-angle) (angle rot)
  (if-let ((abs (absvec rot)))
    (mulv rot (/ angle abs))))

(defun rotation-axis (rot)
  (unitvec rot))

(defun (setf rotation-axis) (axis rot)
  (setv rot (v* (unitvec (/ (absvec rot) (absvec axis))))))

#+nil
(defaccessor rotation-axis- (rot number)
  (aref (rotation-axis rot) number))


(defun r* (rota rotd)
  (let* ((absa (/ (rotation-angle rota) 2)) (absd (/ (rotation-angle rotd) 2))
         (veca (v* (rotation-axis rota) (sin absa))) (vecd (v* (rotation-axis rotd) (sin absd)))
         (cosa (cos absa)) (cosd (cos absd))
         (ang (- (* cosa cosd) (* (scalar veca vecd))))
         (rot (v+ (cross3 veca vecd) (v* veca cosd) (v* vecd cosa))))
    (make-rotation :axis (unitvec rot)
                   :angle (* 2 (realpart (acos ang))))))
 
(defun mulr (rota rotd)
  (setv rota (r* rotd rota)))

(defun rotate-vector (vector rotation)
  (rotation-axis (r*
                  (r* rotation
                      (make-rotation :axis vector :angle pi))
                  (v- rotation))))

(defun rotation-to-vector (front vector)
  (make-rotation :axis (cross3 front vector) :angle (angle front vector)))

(defun vector-rotation (roll side)
  (make-rotation
   :axis (cross3 side roll)
   :angle (line-distance roll side)))

(defun rotation-vector (rot side)
  (v* (cross3 (unitvec side) rot)))


