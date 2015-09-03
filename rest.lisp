#|

(defmacro with-binding (binding (name &optional value) &body body)
  `(symbol-macrolet ((,name ,binding))
     (let ((,binding ,value))
       (declare (special ,binding))
       ,@body)))

(defvar *environment* nil)

(define-symbol-macro .environment.
    (macrolet ((env (&environment env) env))
      (env)))


(defmacro set-vars (&rest bindings)
  `(let ,bindings .environment.))


(defmacro add-vars (&rest bindings)
  (lambda (bindings) (eval #1#))))
(defmacro get-var (var f)
  `(funcall ,f ',var))

(let ((bindings nil))
  (defmacro add-binding (name binding &environment env)
    `(let ((,name ,value))
       e))
  (defmacro get-binding (name)
  (defmacro with-new-binding ((name &optional value) &body)
    (add-binding name (gensym "*BINDING"))
    `(with-old-binding (,name ,value) ,@body))
  (defmacro with-old-binding ((name &optional value) &body body)
    `(with-binding ,(get-binding name) (,name ,value)
       ,@body)))
|#


;;;types?

(deftype satisfies* (function &rest args)
  (let ((fn (gensym "FN")))
    (setf (symbol-function fn)
          (lambda (arg)
            (apply function arg args)))
    `(satisfies ,fn)))

(deftype between (min max)
  `(satisfies* ,(lambda (x) (< min x max))))

(deftype between (min max)
  `(satisfies* (lambda (x a b) (< a x b)) ,min ,max))

(flet ((func-body (body)
         (let ((defer (gensym "DEFER"))
               (recover (gensym "RECOVER")))
           `(let ((,defer '())
                  (,recover nil))
              (block nil
                (handler-case
                    (macrolet ((defer (function) `(push (lambda () ,function) ,',defer))
                               (recover () `(setq ,',recover t)))
                      (unwind-protect
                           (progn ,@body)
                        (dolist (fn ,defer)
                          (funcall fn))))
                  (error (e)
                    (if (not ,recover)
                        (error e)))))))))
  (defmacro defunc (name args &body body)
    `(defun ,name ,args
       ,(func-body body)))
  (defmacro func (args &body body)
    `(lambda ,args
       ,(func-body body))))


(deftype point (&optional n)
  `(vector real ,n))

(deftype points (&rest ns)
  (cons 'or (mapcar (lambda (n) (list 'point n)) ns)))


;;;rest

(let ((fnames (make-hash-table))
      (funcs (make-hash-table)))
  (defmacro l-defun (fname lambda-list &body body)
    (setf (gethash fname fnames) (cons lambda-list body))
    (or (gethash fname funcs) (setf (gethash fname funcs) (make-hash-table))))
  (defmacro l-call (fname &rest args)
    (if (
    (let ((fsym (gensym)))
      `(defun ,fsym ,@(gethash fname fnames))
      

(declaim (inline container value))

(defun container (value)
  (the (simple-vector 1) (vector value)))

(defgeneric add (&rest objects))

(defmethod add (&rest objects)
  (apply #'+ objects))

(defaccessor value (container)
  (svref container 0)
  (type (simple-vector 1) container))

(defun file->list (file)
  (with-open-file (stream file :direction :input)
    (let ((list (container nil)))
      (do* ((element (read stream) (read stream))
            (part (setf (value list) (list element)) (cdr (rplacd part (list element)))))
           ((null (peek-char t stream nil)) (value list))))))

(let ((fnames (make-hash-table))
      (funcs (make-hash-table)))
  (defmacro l-defun (fname lambda-list &body body)
    (setf (gethash fname fnames) (cons lambda-list body))
    (or (gethash fname funcs) (setf (gethash fname funcs) (make-hash-table))))
  (defmacro l-call (fname &rest args)
    (if (
    (let ((fsym (gensym)))
      `(defun ,fsym ,@(gethash fname fnames)))))))

