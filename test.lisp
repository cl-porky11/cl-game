(defun mreader (&optional (stream t) char (num (read stream)))
  (declare (ignore char))
  (let ((symbol (read stream t))
        list)
    (dotimes (i (or num 1) list)
      (push (intern (format nil "~a~a" symbol i)) (reverse list)))))

(set-dispatch-macro-character #\# #\_ #'mreader)
           
(let ((hash-table (make-hash-table :test 'equal)))
  (util:defaccessor name-object (type name)
    (gethash (list type name) hash-table)))

(name-object 'list 'test)

(defun qget (quat)
  (bind-quaternion (r i j k) quat
    (values (* 2 (acos r))
            (vector i j k))))

(defun test-qget (angle axis)
  (qget (quaternion-from-axis-angle axis angle)))




