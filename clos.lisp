#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*package* (find-package :cl)))
    (defun (setf package-nicknames) (list package)
      (rename-package package
                      (package-name package)
                      list)))
  (let ((*package* (find-package #+abcl :mop
                                 #+allegro :mop
                                 #+clisp :clos
                                 #+clozure :ccl
                                 #+cmu :clos-mop
                                 #+ecl :clos
                                 #+lispworks :clos
                                 #+mcl :ccl
                                 #+sbcl :sb-mop
                                 #+scl :clos)))
    (dolist (name '("CLOS" "MOP"))
      (pushnew
       name
       (package-nicknames *package*)
       :test #'string=))))


(defpackage #:clg-clos
  (:use #:cl #:sb-mop #:clg-util))
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

(defun find-programmatic-class-symbol (superclasses)
  (find-programmatic-class (mapfun ((class superclasses)) (find-class class))))

(defun make-programmatic-instance (superclass-names &rest initargs)
  (apply #'make-instance (find-programmatic-class (mapcar #'find-class superclass-names))
         initargs))

(defmethod make-instance ((list list) &rest initargs)
  (if (cdr list)
      (apply #'make-programmatic-instance list initargs)
      (apply #'make-instance (car list))))
