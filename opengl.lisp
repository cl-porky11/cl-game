(defpackage #:cl-game-opengl
  (:nicknames #:clg-gl)
  (:use #:cl #:clg-rot)
  (:export #:with-transformation
           #:rotate-with-rotation
           #:draw-line
           #:draw-circle
           #:draw-primitive
           #:draw-primitives
           ))

(in-package #:clg-gl)

(defparameter *circle-quality* 16)

(defun draw-circle (&optional (quality *circle-quality*))
  (gl:with-pushed-attrib (:current-bit)
    (gl:with-primitive :triangle-fan
      (gl:vertex 0 0)
      (gl:color 0 0 0)
      (dotimes (i (1+ quality))
        (let ((rot (* 2 pi (/ i quality))))
          (gl:vertex (cos rot) (sin rot)))))))

(defun rotate-with-rotation (rot)
  (gl:rotate (/ (* 180 (rotation-angle rot) ) pi)
             (aref rot 0)
             (aref rot 1)
             (aref rot 2)))

(defun draw-line (posa posd)
  (let ((< (>= (length posa) 3)))
    (gl:with-primitive :lines
      (gl:vertex (aref posa 0)
                 (aref posa 1)
                 (if < (aref posa 2) 0))
      (gl:vertex (aref posd 0)
                 (aref posd 1)
                 (if < (aref posd 2) 0)))))

(defmacro with-transformation ((&key translate
                                     scale
                                     rotate)
                               &body body
                               &aux (scale-sym (gensym "SCALE")))
  `(gl:with-pushed-matrix
     ,(if translate `(gl:translate ,@translate))
     ,(if scale
          `(typecase scale
             (list
               (gl:scale ,@scale))
             (integer
               (let ((,scale-sym ,scale))
                 (gl:scale ,scale-sym ,scale-sym ,scale-sym)))))
     ,(if rotate
          `(typecase rotate
                     (integer `(gl:rotate rotate 0 0 1))))
     ,@body))


(defmacro draw-primitive (primitive &body args)
  `(gl:with-primitive ,primitive
     ,@(mapcar (lambda (args) `(gl:vertex ,@args)) args)))

(defmacro draw-primitives (primitive &body args)
  `(draw-primitive ,primitive ,@args))
