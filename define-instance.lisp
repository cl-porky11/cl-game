(defpackage #:define-instance
  (:use #:cl #:sb-mop #:alexandria)
  (:export #:expand-definer-keyword
           #:expand-definer-arguments
           #:define-instance
           #:define-instance-list))
(in-package #:define-instance)

(defgeneric expand-definer-keyword (object keyword entries))

(defgeneric expand-definer-single-arguments (object list))

(defmethod expand-definer-single-arguments (object list)
  list)

(labels ((expand-lists (class lists)
           (if lists
               (destructuring-bind ((keyword . body) . lists) lists
                 `(,keyword ,(expand-definer-keyword class keyword  body) ,@(expand-lists class lists))))))
  (defmacro define-instance (name &rest rest &aux (class (class-prototype (find-class name))))
    (multiple-value-bind (list single) (definer-single-arguments class rest)
      `(make-instance ',name ,@single ,@(expand-lists class list)))))

(defmacro define-instance-list (class &body body)
  `(list ,@(mapcar (lambda  (arg) `(define-findable ,class ,@(ensure-list arg))) body)))
