(defpackage #:text-boxes
  (:use :cl :alexandria :clg-gl))

(defun simple-slot (slot-name &rest options)
    (append (list slot-name :accessor slot-name :initarg (intern (symbol-name slot-name) :keyword)) options))

(defclass box ()
  (#.(simple-slot x :initform 0)
   #.(simple-slot y :initform 0)
   #.(simple-slot w :initform 0)
   #.(simple-slot h :initform 0)
   #.(simple-slot line-color :initform 0)
   #.(simple-slot fill-color :initform 0)
   ))

(defmethod draw ((box box))
  (apply #'gl:color (line-color box))
  (draw-primitives :line-loop
    ((x box) (y box))
    ((x box) (+ (y box) (h box)))
    ((+ (x box) (w box)) (+ (y box) (h box)))
    ((+ (x box) (w box)) (y box)))
  (apply #'gl:color (line-color box)))

(defmethod 













