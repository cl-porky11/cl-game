(defpackage #:eege-util
  (:nicknames #:clg-util)
  (:use #:cl #:alexandria)
  (:export #:defaccessor
           #:with-instances
           #:mulf
           #:construct
           #:funcall-if
           #:apply-if
           #:mapfun
           #:vector-bind
           #:set=
           #:same-elements-p
           #:remove-list
           #:delete-list
           #:remove-duplicates-not
           #:delete-duplicates-not
           #:same-elements-member
           #:subset-member
           #:superset-member
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

(defun applyable (fun)
  (lambda (&rest args)
    (funcall fun args)))

(defun applying (fun)
  (lambda (arg)
    (apply fun arg)))

(defmacro mapfun (args &body body)
  (let ((vars (mapcar #'car args))
        (lists (mapcar #'cadr args)))
    `(mapcar (lambda ,vars ,@body) ,@lists)))

(defmacro construct (fun &rest funs)
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (,fun ,@(mapfun ((fun funs)) `(apply ,fun ,args))))))

#+nil
(defun construct (&rest funs)
  (lambda (&rest args)
    (mapcar (lambda (fun) (apply fun args)) funs)))

#+nil
(defun construct (fun &rest funs)
  (lambda (&rest args)
    (apply fun
           (mapcar (lambda (fun) (apply fun args)) funs))))

(defun set= (seta setd &rest args)
  (and (apply #'subsetp seta setd args)
       (apply #'subsetp setd seta args)))

(defun same-elements-p (list1 list2 &key (test #'eql) test-not key)
  (and (= (length list1) (length list2))
       (or (null list1)
           (same-elements-p (cdr list1)
                             (cdr (remove (car list1) list2
                                          :count 1 :test test :test-not test-not :key key))))))

(defun remove-list (list1 list2 &rest keys)
  (if list1
      (apply #'remove (car list1) (apply #'remove-list (cdr list1) list2 keys) keys)
      list2))

(defun delete-list (list1 list2 &rest keys)
  (if list1
      (apply #'delete (car list1) (apply #'delete-list (cdr list1) list2 keys) keys)
      list2))

(defun remove-duplicates-not (list &rest args)
  (apply #'remove-list (apply #'remove-duplicates list args) list :count 1 args))

(defun delete-duplicates-not (list &rest args)
  (apply #'delete-list (apply #'delete-duplicates list args) list :count 1 args))


(defun table-group (list &key (key #'car) (test #'eql))
  (let ((table (make-hash-table :test test)))
    (dolist (element list)
      (push element (gethash (funcall key element) table)))
    table))

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun false (&rest args)
  (declare (ignore args)))

(defun same-elements-member (item list &rest rest)
  (member-if (lambda (element) (apply #'same-elements-p element item rest))
             list))

(defun subset-member (item list &rest rest)
  (member-if (lambda (element) (apply #'subsetp item element rest))
             list))

(defun superset-member (item list &rest rest)
  (member-if (lambda (element) (apply #'subsetp element item rest))
             list))


