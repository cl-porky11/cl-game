(defpackage #:cl-game-utilities
  (:nicknames #:clg-util)
  (:use #:cl #:alexandria)
  (:export #:defaccessor
           #:define-accessor-method
           #:with-instances
           #:funcall-if
           #:apply-if
           #:dolist*))

(in-package #:clg-util)

;(defmacro define-reader-macro)


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

(defmacro create-instance (class list init-args)
  (with-gensyms (instance)
    `(let ((,instance (apply #'make-instance ,class ,init-args)))
       (push ,instance ,list)
       ,instance)))


(defun method-makunbound (generic-function qualifiers specializers)
  (remove-method generic-function
                 (find-method generic-function qualifiers specializers)))

(defun insert (element list)
  (if list
      (cons (cons element list)
            (mapcar (lambda (rest) (cons (car list) rest))
                    (insert element (cdr list))))
      (list (list element))))

(defun all-permutations (list)
  (if list
      (destructuring-bind (car . cdr) list
        (if cdr
            (apply #'append (mapcar (lambda (list)
                                      (insert car list))
                                    (all-permutations cdr)))
            (list list)))))


(defun different-permutations (list &key (key #'identity) (test #'eql))
    (remove-if-not (let ((specializers '()))
                 (lambda (element) (let ((specializer (mapcar key element)))
                                     (unless (member specializer specializers :test test)
                                       (push specializer specializers)))))
               (all-permutations list) :from-end t))

(defclass changable-generic-function (standard-generic-function)
  ())

(defmacro defmethods (name &rest args)
  `(progn
     (defmethod ,name ,@args)
     (defmethod ,name :before ,@args)
     (defmethod ,name :after ,@args)
     (defmethod ,name :around ,@args)))


#+nil
(defun sub-specializer-p (ma md class)
  (let ((list (class-precedence-list class)))
    (member ma (member md list))))
#+nil
(defun most-specific-specializer (classes method)
  (let ((list (class-precedence-list class)))
    ()))

#+nil
(defun method-more-specific-p (ma md classes)
  (dolist (class (different-permutations classes)
    (if ))))


#+nil
(defmethod compute-applicable-methods-using-classes ((gf changable-generic-function) classes)
  (sort
    (remove-if-not (lambda (method)
                     (some (lambda (classes-permutation)
                             (every #'subclassp
                                    classes-permutation
                                    (method-specializers method)))
                           (different-permutations classes)))
                     (generic-function-methods gf))
    (lambda (ma md)
      (method-more-specific-p ma md classes))))


(defun arguments (lambda-list &optional keyword)
  (if keyword (setq lambda-list (cdr (member keyword lambda-list))))
  (loop for element in lambda-list
       while (not (member element lambda-list-keywords))
       collect element))

(defun bindings (lambda-list)
  (mapcar (lambda (element)
            (if (consp element)
                (if (consp (car element))
                    (cadar element)
                    (car element))
                element))
          lambda-list))

(defun remove-list (items &rest args)
  (apply #'remove-if (lambda (element) (member element items))
         args))
#+nil
(defmacro defmethod* (name &rest args
                      &aux
                        (fname (gensym "FNAME"))
                        qualifiers
                        lambda-list
                        required
                        optional)
  (loop (let ((element (car args)))
          (if (listp element)
              (return)
              (progn
                (push element qualifiers)
                (pop args)))))
  (setq lambda-list (pop args))
  (setq optional lambda-list)
  (loop (let ((element (car optional)))
          (if (or (member element lambda-list-keywords) (null optional))
              (return)
              (progn
                (push element required)
                (pop optional)))))
  (setq required (mapcar (lambda (element)
                           (if (consp element)
                               element
                               (list element t)))
                         required))
  (setq lambda-list (remove-list lambda-list-keywords (bindings lambda-list)))
  `(progn
     (defun ,fname ,lambda-list ,@args)
     ,@(loop for permutation in (different-permutations required :key #'cadr :test #'equal)
          collect `(defmethod ,name ,@(reverse qualifiers) (,@permutation ,@optional)
                             (,fname ,@lambda-list)))))

(defmacro defmethod* (name &rest args
                      &aux
                        qualifiers
                        lambda-list
                        required
                        optional)
  (loop (let ((element (car args)))
          (if (listp element)
              (return)
              (progn
                (push element qualifiers)
                (pop args)))))
  (setq lambda-list (pop args))
  (setq optional lambda-list)
  (loop (let ((element (car optional)))
          (if (or (member element lambda-list-keywords) (null optional))
              (return)
              (progn
                (push element required)
                (pop optional)))))
  (setq required (mapcar (lambda (element)
                           (if (consp element)
                               element
                               (list element t)))
                         required))
  (setq lambda-list (remove-list lambda-list-keywords (bindings lambda-list)))
  `(progn
     ,@(loop for permutation in (different-permutations required :key #'cadr :test #'equal)
          collect `(defmethod ,name ,@(reverse qualifiers) (,@permutation ,@optional)
                             ,@args))))



#+nil
(defmacro defun* (name args &body body)
  (let (
  `(defmacro ,name ))))

#+nil
(defmacro if* (&rest rest) `(if ,@rest))

#+nil
(defmacro (setf if*) (value test then &optional else)
  `(if ,test
     (setf ,then ,value)
     ,(if else
       `(setf ,else value))))


(defmacro dolist* ((var list &optional (rest (gensym "N-LIST"))) &body body &aux (start (gensym "start")))
  `(block nil
     (let ((,rest ,list))
       (tagbody
        ,start
          (unless (endp ,rest)
            (let ((,var (car ,rest)))
              (setq ,rest (cdr ,rest))
              (tagbody
                ,@body))
            (GO ,start))))))


(defun /. (&rest args)
  (if (member :undefined args)
    :undefined
    (if (member :infinity args)
      (if (and (cdr args) (eq (car args) :infinity))
        (if (member :infinity (cdr args))
            :undefined
            :infinity)
        0)
      (handler-case (apply #'/ args)
        (division-by-zero () :infinity)))))

(defun *. (&rest args)
  (if (member :undefined args)
    :undefined
    (if (member :infinity args)
      (if (member 0 args)
        :undefined
        :infinity)
      (apply #'* args))))

(defun +. (&rest args)
  (if (member :undefined args)
    :undefined
    (if (member :infinity args)
      :infinity
      (apply #'+ args))))

(defun -. (&rest args)
  (if (member :undefined args)
    :undefined
    (if (member :infinity args)
      :infinity
      (apply #'- args))))
