(defpackage #:cl-game-utilities
  (:nicknames #:clg-util)
  (:use #:cl #:alexandria)
  (:export #:defaccessor
           #:with-instances
           #:mulf
           #:funcall-if
           #:apply-if
           #:vector-bind
           ))

(in-package #:clg-util)

(defun funcall-if (function &rest args)
  (if function (apply function args)
      (values)))

(defun apply-if (function &rest args)
  (if function (apply #'apply function args)
      (values)))

(defmacro defaccessor (name lambda-list &body body)
  (with-gensyms (value)
    `(progn
       (defun ,name ,lambda-list
         ,@body)
       (defun (setf ,name) ,(cons value lambda-list)
         ,@(butlast body)
         (setf ,@(last body) ,value)))))

(defmacro with-instances (default-class instances &body body)
  `(let* ,(mapcar (lambda (instance)
                    (destructuring-bind (name &optional (class default-class) &rest rest)
                        (ensure-list instance)
                      `(,name (make-instance ',class ,@rest))))
                  instances)
     ,@body))

(defmacro vector-bind (list vec &body body &aux (i -1))
  (with-gensyms (vector)
    `(let ((,vector ,vec))
       (let ,(mapcar (lambda (var) `(,var (aref ,vector ,(incf i)))) list)
         ,@body))))

(define-modify-macro mulf (pos &rest args) *)
