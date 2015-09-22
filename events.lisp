(defpackage #:clg-events
  (:use #:cl #:clg-util #:sb-mop #:alexandria #:define-instance)
  (:export #:story
           #:story-event
           
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

(defclass story ()
  ((active :initarg :active :initform nil)
   (finished :initarg :finished :initform (make-hash-table :test #'eq))
   (selector :initarg :selector :initform #'car)
   (events :initform (make-hash-table :test #'eq))
   (undefined :initform nil))
  (:metaclass funcallable-standard-class))

(defvar *story* (make-instance 'story))

(defaccessor story-event (name &optional (story *story*))
  (gethash name (slot-value story 'events)))

(defmethod initialize-instance :after ((story story) &key)
  (set-funcallable-instance-function
   story
   (lambda (&aux (*story* story))
     (with-slots (active selector) *story*
       (call-event (funcall selector (reverse active)))
       active))))

(defun next-event-undefined (story event)
  (with-slots (undefined) story
    (pushnew event undefined :test #'eq)))

(defclass event () ())

(defun ensure-activate-event (next)
  (let ((active (event-active-p next))
        (callable (event-callable-p* next)))
    (cond
      ((and callable (not active))
       (activate-event next))
      ((and (not callable) active)
       (deactivate-event next)))))

(defmethod initialize-instance :after ((event event) &key name)
  (with-slots (undefined) story
    (setf (story-event name) event)
    (deletef name undefined-events)
    (ensure-activate-event name)))

(defmethod expand-definer-single-arguments ((obj event) list)
  (destructuring-bind (name . rest) list
    (values rest
            `(:name ,name))))


(defun call-event (name)
  (with-slots (active) *story*
    (disable-event name)
    (deletef active name :count 1)
    (dolist (next (ensure-list (event-select name)))
      (if (symbolp next)
          (if (story-event next)
              (progn
                (enable-event name next)
                (ensure-activate-event next))
              (next-event-undefined *story* next))
          (warn "The selector of the event should only return lists of symbols or symbols~%~\
                 You returned the ~a ~a"
                (type-of next) next)))
    (if (and (event-callable-p* name) (not (event-active-p name)))
        (activate-event name))))


(defun event-active-p (name)
  (with-slots (active) *story*
    (member name active)))


(defgeneric event-activate (event))

(defmethod event-activate ((name symbol))
  (event-activate (story-event name)))

(defmethod event-activate ((event event)))

(defun activate-event (name)
  (with-slots (active) *story*
    (push name active)
    (event-activate name)))


(defgeneric event-deactivate (event))

(defmethod event-deactivate ((name symbol))
  (event-deactivate (story-event name)))

(defmethod event-deactivate ((event event)))

(defun deactivate-event (name)
  (with-slots (active) *story*
    (deletef active name :count 1)
    (event-deactivate name)))


(defgeneric event-disable (event list))

(defmethod event-disable ((name symbol) list)
  (event-disable (story-event name) list))

(defmethod event-disable ((event event) list))

(defun disable-event (name)
  (with-slots (finished) *story*
    (setf (gethash name finished)
          (event-disable name (gethash name finished)))))


(defgeneric event-enable (event new list))

(defmethod event-enable ((name symbol) new list)
  (event-enable (story-event name) new list))

(defmethod event-enable ((event event) new list)
  (cons new list))

(defun enable-event (new name)
  (with-slots (finished) *story*
    (setf (gethash name finished)
          (event-enable name
                        new (gethash name finished)))))


(defgeneric event-callable-p (event list))

(defmethod event-callable-p ((event symbol) list)
  (event-callable-p (story-event event) list))

(defmethod event-callable-p ((event event) list))

(defun event-callable-p* (name)
  (with-slots (finished) *story*
    (event-callable-p name (gethash name finished))))


(defgeneric event-select (event))

(defmethod event-select ((event symbol))
  (event-select (story-event event)))

(defmethod event-select ((event event)))

(defmethod expand-definer-keyword ((instance event) keyword entries)
  (destructuring-bind (car . cdr) entries
    (etypecase car
      (list `(lambda ,car ,@cdr))
      (symbol `(,car ,@(mapfun ((arg cdr)) `',arg))))))


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

(defmethod expand-definer-keyword ((instance event) (keyword (eql :after)) entries)
  `',entries)

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

(defclass substory (event)
  (story))

(defmethod event-select ((event substory))
  (loop for result = (funcall *story*)
     unless result
     return (slot-value *story* 'undefined)))


