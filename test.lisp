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

(defun vector-angle (roll curr)
  (quat:quaternion-from-axis-angle
   (clg-vec:cross3 roll curr)
   (clg-vec:line-distance roll curr)))

(defun precise (number &optional (divisor 1))
  (* (round number divisor) divisor))

(defun precise-call (divisor function &rest args)
  (precise (apply function args) divisor))

(defun transformed-call (factor function &rest args)
  (/ (apply function (mapcar (lambda (arg) (* arg factor)) args)) factor))


#+nil
(with-open-file (in "test.txt" :direction :input)
  (with-open-file (out "x.txt" :direction :output :if-does-not-exist :create)
    (loop
       (unless (numberp (read in nil))
         (return))
       (write-char (read-char in) out)
       (read-line in))))

(declaim (optimize (speed 3)))

(declaim (ftype (function (&rest t) (t)) middle)
         (inline middle))
(defun middle (&rest args)
  (nth (floor (/ (length args) 2)) (sort args '<)))

;(loop for x in #2a((1 2) (3 4)) do (print x))


