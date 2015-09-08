(defvar *

(set-pprint-dispatch t (lambda (x y) (print-object y x)))

(defmethod print-object ((object t) s)
  (funcall (pprint-dispatch object nil) s object)
  object)

(defvar *list-print-table* (make-hash-table))

(defmacro define-list-printer (car var &body body)
  `(eval-when (:load-toplevel :compile-toplevel :eval)
     (setf (gethash ,car ?)
           (lambda (,var)
             ,@body))))

(let ((simple-print-list #:simple-print-list))
(defun print-list (list stream)
  (if-let ((f (gethash (car list) *list-print-table*)))
    (funcall f (cdr list) stream)
    (some-normal-print-list)


(defmethod print-object ((object list) s)
  (print-list object s))
