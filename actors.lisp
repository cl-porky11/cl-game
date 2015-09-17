(defpackage #:clg-act
  (:use #:cl #:clg-util #:clg-spat #:alexandria)
  (:export #:actor))
(in-package #:clg-act)

(defclass actor ()
  ((action :initform nil :initarg :action :type (or function null))))

(defmethod act progn ((actor actor))
  (with-slots (action) actor
    (funcall-if action actor)))

(defun add-action (new-action actor &key before)
  (with-slots (action) actor
    (setf action (if-let ((old-action action))
                   (if before
                       (construct progn new-action old-action)
                       (construct progn old-action new-action))
                   new-action))))

;;;persons

#|
(defclass person (mover actor)
  ((acc :reader acc :initarg :acc :type number)))

(defmethod move-to ((person person) (where positional))
  (act-before person
    (lambda (person)
      (accelerate-to person where (acc person)))))



(defclass actor ()
  (action))

(defmethod act (act

(defclass person ()
  (goal))
|#

