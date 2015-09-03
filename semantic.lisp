(defpackage #:semantic
  (:use #:cl #:alexandria #:closer-mop)
  (:export
   #:make-simple-read-table
   #:double-reading-function
   #:macro-character
   #:simple-read
   #:*simple-read-table*
   #:char-reader
   #:single-escape-reader
   #:string-reader
   #:symbol-reader
   #:simple-read-string
   #:extended-read-string
   ))

(in-package #:semantic)


(defmacro defaccessor (name lambda-list &body body)
  (with-gensyms (value)
    `(progn
       (defun ,name ,lambda-list
         ,@body)
       (defun (setf ,name) ,(cons value lambda-list)
         ,@(butlast body)
         (setf ,@(last body) ,value)))))

(defun apply-if (function &rest args)
  (if function (apply #'apply function args)
      (values)))

(defun string-pointer (string &optional (start 0) (end nil))
  (make-array (- (or end (length string)) start)
              :displaced-to string
              :displaced-index-offset start
              :fill-pointer end
              :element-type 'character))

(defclass context ()
  ((table :initform (make-hash-table))
   (function :initform 'context-read :initarg :function))
  (:metaclass funcallable-standard-class))

(defmacro with-context (context &body body)
  `(let ((*current-context* ,context)) ,@body))

(defmethod initialize-instance :after ((context context) &key)
  (with-slots (table function) context
    (set-funcallable-instance-function context
                                       (lambda (&rest args)
                                         (with-context context
                                           (apply-if function args))))))

(defmethod next-context ((context context) input)
  (with-slots (table) context
    (or (gethash input table) (no-context context))))

(defmethod no-context ((context context))
  (make-instance 'context :function nil))

(defclass null-context (context) ()
  (:metaclass funcallable-standard-class))

(defvar *current-context*)

(setq *current-context* (make-instance 'null-context))

(defaccessor char-context (char &optional (context *current-context*))
  (gethash char (slot-value context 'table)))

(defun string-context (string &optional (context *current-context*))
  (if (string/= "" string)
      context
      (string-context (string-pointer string 1)
                      (or (gethash (char string 0) (slot-value context 'table))
                          (return-from string-context)))))

(defun (setf string-context) (value string &optional (context *current-context*))
  (let ((newhash (ensure-gethash (char string 0) (slot-value context 'table)
                                 (if (= 1 (length string)) value (make-instance 'context)))))
    (if (= 1 (length string))
        (setf (string-context (string-pointer string 1)
                              newhash)
              value))))

(defun context-read (&optional (stream t) &aux (char (read-char stream)))
  (funcall (next-context *current-context* char)) char)

(setf (string-context "Test") (make-instance 'context
                                             :function (lambda (&optional char)
                                                         (declare (ignore char))
                                                         (print "Test erfolgreich!"))))

(defun simple-read (&optional (stream t) (ignored-char) &rest args &aux (char (read-char stream)))
  (declare (ignore ignored-char))
  (apply-if (or (char-context char) (char-context nil))
            stream char
            args))

(


(labels ((simple-plist (plist)
           (if plist
             (destructuring-bind (car cadr . cddr) plist
               (append (mappend (lambda (key)
                                  (list key cadr))
                                (ensure-cons car))
                       (simple-plist cddr))))))
  (defun make-simple-read-table (read-table &rest chars)
    (plist-hash-table (append (if read-table (hash-table-plist read-table)) (simple-plist chars)))))

(defparameter *character-table* ())

(defaccessor character-function (char &optional (table *character-table*))
  (gethash char table))


(defun simple-read (&optional (stream t) (ignored-char) &rest args &aux (char (read-char stream)))
  (declare (ignore ignored-char))
  (

(defstruct 
  function table)

(defgeneric set-character-functions (char function &key))

(defmethod set-character-functions ((char character) function &key (table *character-table*))
  (setf (character-function char table) function))

(defmethod set-character-functions ((char null) function &key (table *character-table*))
  (setf (character-function nil table) function))

(defmethod set-character-functions ((list cons) function &key (table *character-table*))
  (dolist (char list)
    (set-character-functions char function)))

(defmethod set-character-functions ((string string) function &key (table *character-table*)
                                    &aux (table (or (character-fun(make-character-table)
  (set-character-functions (char string 0)



(defgeneric keyword-action (keyword &rest keywords))

(defmacro make-action (keyword &rest keywords)
  (apply 'keyword-action keyword keywords))

(defmacro define-single-method (name type lambda-list &body body)
  `(defmethod ,name ((,(gensym "VAR") (eql ',type)) ,@lambda-list)
     ,@body))

(define-single-method keyword-action go (keywords)
  `(go (keyword-action ,(car keywords) ,(cdr keywords))))

(define-single-method keyword-action person (keywords)

  )


(defclass person (entity))
(defclass attacker () ())

(defmacro bla (arg &rest args)
  `

  a und b gehen zu c
 => (c (zu (gehen (b (und a)))))

 a: a
 (und <a>): und
 (b <und>): 
  

(defclass entity () ())


(defclass bunch () (list))
(defmethod f ((b bunch) &rest)
  (dolist (e b-list)
    (f e)))

(defgeneric focus (name last-object))

(deffocus ((name symbol) last-object)
  (focus (funcall (selector name)) last-object))

(deffocus ((entity string) (last-object null))
  entity)

(deffocus ((entity string) (last-object list))
  (cons entity last-object))


(defselector hans
    "hans")

(defselector bert
    "bert")

(defselector and
    nil)

(deffocus ((and list) last-object)
  (cons last-object and))
(deffocus ((and null) last-object)
  (cons last-object and))

(defmacro deffocus (object last-object &body body)
  `(defmethod focus (,object ,last-object)
     ,@body))


(defun focus-list (list)
  (if list
      `(focus ',(car list) ,(focus-list (cdr list)))))

(defmacro focusing (&rest args)
  (focus-list (reverse args)))


(let ((table (make-hash-table)))
  (defaccessor selector (name)
    (gethash name table)))

(defmacro defselector (name &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (selector ',name) (lambda () ,@body))))

