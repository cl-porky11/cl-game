(defpackage #:clg-events
  (:use #:cl #:clg-util #:findable #:alexandria)
  (:export #:ensure-event
           #:define-event
           #:call-story

           #:event
           #:selector-event
           #:selector
           #:prequal-event
           #:prequels
           #:activator-event
           #:activator
           #:deactivator-event
           #:deactivator
           #:standard-event

           #:event-enable
           #:event-callable-p
           #:event-disable
           #:event-activate
           #:event-deactivate
           #:event-select
           ))
(in-package #:clg-events)

(defvar *active-events*)

(defclass event (findable-object) ()
  (:metaclass findable-class))


(defvar *active-events*)

(defun event-active-p (name)
  (member name *active-events*))


(defgeneric event-activate (event))

(defmethod event-activate ((name symbol))
  (event-activate (find-instance 'event name)))

(defmethod event-activate ((event event)))

(defun activate-event (name)
  (push name *active-events*)
  (event-activate name))


(defgeneric event-deactivate (event))

(defmethod event-deactivate ((name symbol))
  (event-deactivate (find-instance 'event name)))

(defmethod event-deactivate ((event event)))

(defun deactivate-event (name)
  (deletef *active-events* name :count 1)
  (event-deactivate name))


(defvar *finished-table*)


(defgeneric event-disable (event list))

(defmethod event-disable ((name symbol) list)
  (event-disable (find-instance 'event name) list))

(defmethod event-disable ((event event) list))

(defun disable-event (name)
  (setf (gethash name *finished-table*)
        (event-disable name (gethash name *finished-table*))))


(defgeneric event-enable (event new list))

(defmethod event-enable ((name symbol) new list)
  (event-enable (find-instance 'event name) new list))

(defmethod event-enable ((event event) new list)
  (cons new list))

(defun enable-event (new name)
  (setf (gethash name *finished-table*)
        (event-enable name
                      new (gethash name *finished-table*))))


(defgeneric event-callable-p (event list))

(defmethod event-callable-p ((event symbol) list)
  (event-callable-p (find-instance 'event event) list))

(defmethod event-callable-p ((event event) list))

(defun event-callable-p* (name)
  (event-callable-p name (gethash name *finished-table*)))


(defgeneric event-select (event))

(defmethod event-select ((event symbol))
  (event-select (find-instance 'event event)))

(defmethod event-select ((event event)))

(defun call-event (name)
  (disable-event name)
  (deletef *active-events* name :count 1)
  (dolist (next (ensure-list (event-select name)))
    (enable-event name next)
    (if (symbolp next)
        (let ((active (event-active-p next))
              (callable (event-callable-p* next)))
          (cond
            ((and callable (not active))
             (activate-event next))
            ((and (not callable) active)
             (deactivate-event next))))
        (warn "The selector of the event should only return lists of symbols or symbols~%~\
                 You returned the ~a ~a"
              (type-of next) next)))
  (if (and (event-callable-p* name) (not (event-active-p name)))
      (activate-event name)))


(defun ensure-event (name class &rest args)
  (apply #'make-instance class :name name args))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((expand-function (function)
             (destructuring-bind (car . cdr) function
               (etypecase car
                 (list `(lambda ,car ,@cdr))
                 (symbol `(,car ,@(mapfun ((arg cdr)) `',arg))))))
           (expand-lists (lists)
             (if lists
                 (destructuring-bind ((keyword . body) . lists) lists
                   `(,keyword ,(expand-function body) ,@(expand-lists lists))))))
    (defmacro define-event (name class &rest lists)
      `(ensure-event ',name ',class ,@(expand-lists lists)))))

(defun call-story (active &key (selector #'car)
                            state
                            (reload (not state))
                   &aux (*finished-table* (or state (make-hash-table :test #'eq)))
                     (*active-events* (unless reload (ensure-list active))))
  (when reload
    (dolist (active (ensure-list active))
      (activate-event active)))
  (call-event (funcall selector (reverse *active-events*)))
  (values *active-events* *finished-table*))

(defclass selector-event (event)
  ((selector :initarg :selector :initform nil)))

(defmethod event-select ((event selector-event))
  (with-slots (selector) event
    (funcall-if selector)))

(defclass prequel-event (event)
  ((prequels :initform nil)))

(defmethod initialize-instance :after ((event prequel-event) &key after)
  (setf (slot-value event 'prequels) (mapcar #'ensure-list after)))

(defmethod event-callable-p ((event prequel-event) list)
  (with-slots (prequels) event
    (same-elements-member list prequels :test #'equal)))

(defclass activator-event (event)
  ((activator :initarg :activator :initform nil)))

(defmethod event-activate ((event activator-event))
  (with-slots (activator) event
    (funcall-if activator)))

(defclass deactivator-event (event)
  ((deactivator :initarg :deactivator :initform nil)))

(defmethod event-deactivate ((event deactivator-event))
  (with-slots (deactivator) event
    (funcall-if deactivator)))

(defclass standard-event (selector-event prequel-event activator-event deactivator-event) ())




