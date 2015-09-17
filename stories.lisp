(defpackage #:clg-stories
  (:use #:cl #:clg-util #:alexandria)
  (:export #:ensure-story
           #:story-name
           #:find-story
           #:delete-story
           #:define-story
           #:start-with-stories

           #:story
           #:selector-story
           #:prequal-story
           #:activator-story
           #:deactivator-story
           #:standard-story

           #:story-activate
           #:story-deactivate
           #:story-disable
           #:story-enable
           #:story-callable-p
           #:story-select
           ))
(in-package #:clg-stories)

(defvar *active-stories*)

(defclass story ()
  ((name :initarg :name)))

(defun story-name (story-name)
  (etypecase story-name
    (story (slot-value story-name 'name))
    (symbol story-name)))

(let ((table (make-hash-table :test #'eq)))
  (defun find-story (story-name)
    (etypecase story-name
      (symbol
       (values (gethash story-name table) story-name))
      (story
       (values story-name (slot-value story-name 'name)))))
  (defun set-story (name story)
    (check-type name symbol)
    (check-type story story)
    (setf (gethash name table) story))
  (defun delete-story (story-name)
    (remhash (slot-value (find-story story-name) 'name)
             table)))

(defvar *active-stories*)

(defun story-active-p (story-name)
  (member (story-name (find-story story-name)) *active-stories*))


(defgeneric story-activate (story))

(defmethod story-activate ((name symbol))
  (story-activate (find-story name)))

(defmethod story-activate ((story story)))

(defun activate-story (story-name)
  (let ((name (story-name story-name)))
    (push name *active-stories*)
    (story-activate name)))


(defgeneric story-deactivate (story list))

(defmethod story-deactivate ((name symbol) list)
  (story-deactivate (find-story name) list))

(defmethod story-deactivate ((story story) list))

(defun deactivate-story (story-name)
  (let ((name (story-name story-name)))
    (deletef *active-stories* name :count 1)
    (story-deactivate name)))


(defvar *finished-table*)


(defgeneric story-disable (story list))

(defmethod story-disable ((name symbol) list)
  (story-disable (find-story name) list))

(defmethod story-disable ((story story) list))

(defun disable-story (story-name)
  (let ((name (story-name story-name)))
    (setf (gethash name *finished-table*)
          (story-disable name (gethash name *finished-table*)))))


(defgeneric story-enable (story new list))

(defmethod story-enable ((name symbol) new list)
  (story-enable (find-story name) new list))

(defmethod story-enable ((story story) new list)
  (cons new list))

(defun enable-story (new story-name)
  (let ((name (story-name story-name)))
    (setf (gethash name *finished-table*)
          (story-enable name
                        new (gethash name *finished-table*)))))


(defgeneric story-callable-p (story list))

(defmethod story-callable-p ((story symbol) list)
  (story-callable-p (find-story story) list))

(defmethod story-callable-p ((story story) list))

(defun story-callable-p* (story-name)
  (let ((name (story-name story-name)))
    (story-callable-p name (gethash name *finished-table*))))


(defgeneric story-select (story))

(defmethod story-select ((story symbol)))

(defmethod story-select ((story story)))

(defun call-story (story-name)
  (multiple-value-bind (story name) (find-story story-name)
    (disable-story story)
    (deletef *active-stories* name :count 1)
    (dolist (next (ensure-list (story-select story)))
      (enable-story name next)
      (if (symbolp next)
          (let ((active (story-active-p next))
                (callable (story-callable-p* next)))
            ;;(print (list name :active active :callable callable))
            (cond
              ((and callable (not active))
               (activate-story next))
              ((and (not callable) active)
               (deactivate-story next))))
          (warn "The selector of the story should only return lists of symbols or symbols~%~\
                 You returned the ~a ~a"
                (type-of next) next)))
    (if (and (story-callable-p* story) (not (story-active-p story)))
        (activate-story story))))


(defun ensure-story (name class &rest args)
  (set-story name
             (apply #'make-instance class :name name args)))


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
    (defmacro define-story (name (&optional (class 'standard-story)) &rest lists)
      `(ensure-story ',name ',class ,@(expand-lists lists)))))

(defun start-with-stories (active &optional (selector #'car)
                           &aux (*finished-table* (make-hash-table :test #'eq))
                             *active-stories*)
  (dolist (active (ensure-list active))
    (activate-story active))
  (loop while *active-stories*
     do (call-story (funcall selector (reverse *active-stories*)))))


(defclass selector-story (story)
  ((selector :initarg :selector :initform nil)))

(defmethod story-select ((story selector-story))
  (with-slots (story-selector) story
    (funcall-if story-selector story)))

(defclass prequel-story (story)
  ((prequels :initform nil)))

(defmethod initialize-instance ((story prequel-story) &key after)
  (setf (slot-value story 'prequels) (mapcar #'ensure-list after)))

(defmethod story-callable-p ((story prequel-story) list)
  (with-slots (prequels) story
    (same-elements-member list prequels :test #'equal)))

(defclass activator-story (story)
  ((activator :initarg :activator :initform nil)))

(defmethod story-select ((story activator-story))
  (with-slots (story-activator) story
    (funcall-if story-activator story)))

(defclass deactivator-story (story)
  ((deactivator :initarg :deactivator :initform nil)))

(defmethod story-select ((story selector-story))
  (with-slots (story-deactivator) story
    (funcall-if story-deactivator story)))

(defclass standard-story (selector-story prequel-story activator-story deactivator-story) ())




