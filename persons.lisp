
(defclass actor ()
  ((action :accessor action :initarg :action :type (or function null))))

(defmethod act :before ((actor actor))
  (if (action actor)
    (funcall (action actor) actor)))

(defmethod act ((actor actor)))

(defun combine-actions (first second actor)
  (declare (type (or function null) first second) (type actor actor))
  (if first
    (funcall first actor)
    (unless (action actor)
      (setf (action actor) second))))

(defun act-before (actor function)
  (setf (action actor) (lambda (entity) (combine-actions function (action actor) entity))))

(defun act-after (actor function)
  (setf (action actor) (lambda (entity) (combine-actions (action actor) function entity))))

;;;persons

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


