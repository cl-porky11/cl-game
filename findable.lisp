(defpackage #:findable
  (:use #:cl #:sb-mop #:alexandria)
  (:export #:findable-class
           #:find-instance
           #:set-instance
           #:findable-object
           #:name-of
           #:delete-instance))
(in-package #:findable)

(defclass findable-class (standard-class)
  ((name-table :initform (make-hash-table :test #'eq))))

(defun find-instance (class-name object-name)
  (let ((class (find-class class-name)))
    (check-type class findable-class)
    (if (eq (class-of object-name) class)
        object-name
        (gethash object-name (slot-value class 'name-table)))))

(defun class-findable-p (class)
  (member (find-class 'findable-object) (class-direct-superclasses class)))

(defun class-findable-classes (class)
  (remove-if-not #'class-findable-p (class-precedence-list class)))

(defclass findable-object ()
  ((name :initarg :name :type symbol)
   (findable-classes)))

(defun set-instance (name object)
  (check-type name symbol)
  (dolist (class (slot-value object 'findable-classes))
    (let ((table (slot-value class 'name-table)))
      (setf (gethash name table) object))))


(defun name-of (name-object)
  (if (symbolp name-object)
      name-object
      (slot-value name-object 'name)))

(defmethod initialize-instance :after ((object findable-object) &key)
  (unless (slot-boundp object 'findable-classes)
    (setf (slot-value object 'findable-classes)
          (class-findable-classes (class-of object))))
  (set-instance (name-of object) object))

(defun delete-instance (object)
  (let ((class (slot-value object 'findable-superclass)))
    (remhash (name-of object)
             (slot-value class 'name-table))))

(defmethod initialize-instance :around ((class findable-class) &rest args)
  (appendf (getf args :direct-superclasses) (list (find-class 'findable-object)))
  (apply #'call-next-method class args))

(defmethod validate-superclass ((class findable-class) (super standard-class)) t)
(defmethod validate-superclass ((class standard-class) (super findable-class)) t)


