(defpackage #:stories
  (:use #:cl)
  (:export #:*language*
           #:add-story
           #:list-stories
           #:set-story
           #:person))

(defpackage #:stories-test
  (:use #:stories))

;(in-package #:stories)

(defvar *language* :de)

(defun somehash (hash-table)
  (maphash (lambda (key val)
             (declare (ignore key))
             (return-from somehash val))
           hash-table)
  nil)

(defun locale (text &optional (language *language*))
  (or (gethash language text)
      (gethash :en text)
      (somehash text)))

(defun (setf locale) (value text &optional (language *language*))
  (setf (gethash language text) value))

(defmacro with-input-string (string &body body)
  `(let ((*standard-input*
          (make-concatenated-stream (make-string-input-stream ,string)
                                    *standard-input*)))
     ,@body))


(defclass story ()
  ((description :initform (make-hash-table :test 'eq))
   (text :initform (make-hash-table :test 'eq))
   (sequals :initform (make-hash-table :test 'equal))))

(defvar *story* (make-instance 'story))

(define-symbol-macro stories (slot-value *story* 'sequals))

(defun add-story (&optional (name (read-line)) &aux (story (make-instance 'story)))
  (setf (gethash name stories) story)
  (setf (locale (slot-value story 'description)) (with-input-string "\"" (read)))
  (setf (locale (slot-value story 'text)) (with-input-string "\"" (read)))
  (values))

(defun list-stories ()
  (maphash
   (lambda (key value)
     (format t "~a:~%~a~%~%" key (locale (slot-value value 'description))))
   stories)
  (values))

(defun set-story (&optional (name (read-line)) &aux (story (read-line)))
  (setq *story* (gethash name stories)))

(defun tell-story (story)
  (write-line (slot-value 'text story)))


(defclass person ()
  ())

(define-modify-macro appendf (value &rest rest) append)

(let ((values (make-hash-table)))
  (defun translate-reader (stream char &aux (text (read stream)) (key (read stream)))
    (declare (ignore char))
    (setf (gethash key values) text)
    `(funcall ,(lambda () (gethash key values))))
  (defmacro locale (text key)
    (setf (gethash key values) text)
    `(funcall ,(lambda () (gethash key values))))
  (defun set-locale (text key)
    (setf (gethash key values) text)))

(defun print-text ()
  (print $"Hallo" a)
  (print $"Ich bin ein Junge" b)
  (print $"Was bist du?" c))

(defun set-locales (&rest list)
  (dolist (a list)
    (set-locale (car a) (cadr a))))

(set-locales '("Hello" a) '("I'm a boy" b) '("What are you?" c))

(let ((strings (make-hash-table)))

(set-macro-character #\$ 'translate-reader)

