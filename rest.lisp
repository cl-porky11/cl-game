(defmacro create-instance (class list init-args)
  (with-gensyms (instance)
    `(let ((,instance (apply #'make-instance ,class ,init-args)))
       (push ,instance ,list)
       ,instance)))


(defun method-makunbound (generic-function qualifiers specializers)
  (remove-method generic-function
                 (find-method generic-function qualifiers specializers)))

(defun insert (element list)
  (if list
      (cons (cons element list)
            (mapcar (lambda (rest) (cons (car list) rest))
                    (insert element (cdr list))))
      (list (list element))))

(defun all-permutations (list)
  (if list
      (destructuring-bind (car . cdr) list
        (if cdr
            (apply #'append (mapcar (lambda (list)
                                      (insert car list))
                                    (all-permutations cdr)))
            (list list)))))


(defun different-permutations (list &key (key #'identity) (test #'eql))
    (remove-if-not (let ((specializers '()))
                 (lambda (element) (let ((specializer (mapcar key element)))
                                     (unless (member specializer specializers :test test)
                                       (push specializer specializers)))))
               (all-permutations list) :from-end t))

(defclass changable-generic-function (standard-generic-function)
  ())

(defmacro defmethods (name &rest args)
  `(progn
     (defmethod ,name ,@args)
     (defmethod ,name :before ,@args)
     (defmethod ,name :after ,@args)
     (defmethod ,name :around ,@args)))


#+nil
(defun sub-specializer-p (ma md class)
  (let ((list (class-precedence-list class)))
    (member ma (member md list))))
#+nil
(defun most-specific-specializer (classes method)
  (let ((list (class-precedence-list class)))
    ()))

#+nil
(defun method-more-specific-p (ma md classes)
  (dolist (class (different-permutations classes)
    (if ))))


#+nil
(defmethod compute-applicable-methods-using-classes ((gf changable-generic-function) classes)
  (sort
    (remove-if-not (lambda (method)
                     (some (lambda (classes-permutation)
                             (every #'subclassp
                                    classes-permutation
                                    (method-specializers method)))
                           (different-permutations classes)))
                     (generic-function-methods gf))
    (lambda (ma md)
      (method-more-specific-p ma md classes))))


(defun arguments (lambda-list &optional keyword)
  (if keyword (setq lambda-list (cdr (member keyword lambda-list))))
  (loop for element in lambda-list
       while (not (member element lambda-list-keywords))
       collect element))

(defun bindings (lambda-list)
  (mapcar (lambda (element)
            (if (consp element)
                (if (consp (car element))
                    (cadar element)
                    (car element))
                element))
          lambda-list))

(defun remove-list (items &rest args)
  (apply #'remove-if (lambda (element) (member element items))
         args))
#+nil
(defmacro defmethod* (name &rest args
                      &aux
                        (fname (gensym "FNAME"))
                        qualifiers
                        lambda-list
                        required
                        optional)
  (loop (let ((element (car args)))
          (if (listp element)
              (return)
              (progn
                (push element qualifiers)
                (pop args)))))
  (setq lambda-list (pop args))
  (setq optional lambda-list)
  (loop (let ((element (car optional)))
          (if (or (member element lambda-list-keywords) (null optional))
              (return)
              (progn
                (push element required)
                (pop optional)))))
  (setq required (mapcar (lambda (element)
                           (if (consp element)
                               element
                               (list element t)))
                         required))
  (setq lambda-list (remove-list lambda-list-keywords (bindings lambda-list)))
  `(progn
     (defun ,fname ,lambda-list ,@args)
     ,@(loop for permutation in (different-permutations required :key #'cadr :test #'equal)
          collect `(defmethod ,name ,@(reverse qualifiers) (,@permutation ,@optional)
                             (,fname ,@lambda-list)))))

(defmacro defmethod* (name &rest args
                      &aux
                        qualifiers
                        lambda-list
                        required
                        optional)
  (loop (let ((element (car args)))
          (if (listp element)
              (return)
              (progn
                (push element qualifiers)
                (pop args)))))
  (setq lambda-list (pop args))
  (setq optional lambda-list)
  (loop (let ((element (car optional)))
          (if (or (member element lambda-list-keywords) (null optional))
              (return)
              (progn
                (push element required)
                (pop optional)))))
  (setq required (mapcar (lambda (element)
                           (if (consp element)
                               element
                               (list element t)))
                         required))
  (setq lambda-list (remove-list lambda-list-keywords (bindings lambda-list)))
  `(progn
     ,@(loop for permutation in (different-permutations required :key #'cadr :test #'equal)
          collect `(defmethod ,name ,@(reverse qualifiers) (,@permutation ,@optional)
                             ,@args))))



#+nil
(defmacro defun* (name args &body body)
  (let (
  `(defmacro ,name ))))

#+nil
(defmacro if* (&rest rest) `(if ,@rest))

#+nil
(defmacro (setf if*) (value test then &optional else)
  `(if ,test
     (setf ,then ,value)
     ,(if else
       `(setf ,else value))))


(defmacro dolist* ((var list &optional (rest (gensym "N-LIST"))) &body body &aux (start (gensym "start")))
  `(block nil
     (let ((,rest ,list))
       (tagbody
        ,start
          (unless (endp ,rest)
            (let ((,var (car ,rest)))
              (setq ,rest (cdr ,rest))
              (tagbody
                ,@body))
            (GO ,start))))))


(defun /. (&rest args)
  (if (member :undefined args)
    :undefined
    (if (member :infinity args)
      (if (and (cdr args) (eq (car args) :infinity))
        (if (member :infinity (cdr args))
            :undefined
            :infinity)
        0)
      (handler-case (apply #'/ args)
        (division-by-zero () :infinity)))))

(defun *. (&rest args)
  (if (member :undefined args)
    :undefined
    (if (member :infinity args)
      (if (member 0 args)
        :undefined
        :infinity)
      (apply #'* args))))

(defun +. (&rest args)
  (if (member :undefined args)
    :undefined
    (if (member :infinity args)
      :infinity
      (apply #'+ args))))

(defun -. (&rest args)
  (if (member :undefined args)
    :undefined
    (if (member :infinity args)
      :infinity
      (apply #'- args))))

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

