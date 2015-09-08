(defpackage #:clg-clos
  (:use #:cl #:sb-mop))
(in-package #:clg-clos)

(defun make-programmatic-class (superclasses)
  (make-instance 'standard-class
                 :name (mapcar #'class-name superclasses)
                 :direct-superclasses superclasses
                 :direct-slots nil))

(defun find-programmatic-class (superclasses)
  (or (find-if
       (lambda (class)
         (equal superclasses (class-direct-superclasses class)))
       (class-direct-subclasses (car superclasses)))
      (make-programmatic-class superclasses)))

(defun make-programmatic-instance (superclass-names &rest initargs)
  (apply #'make-instance (find-programmatic-class (mapcar #'find-class superclass-names))
         initargs))

(defmethod make-instance ((list list) &rest initargs)
  (if (cdr list)
      (apply #'make-programmatic-instance list initargs)
      (apply #'make-instance (car list))))
