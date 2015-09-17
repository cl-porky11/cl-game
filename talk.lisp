(defpackage #:clg-talk
  (:use #:cl #:clg-named #:alexandria)
  (:export #:say
           ))

(in-package #:clg-talk)

(defun tell (string)
  (format t "~a~%" string)
  nil)

(defun ask (string)
  (tell string)
  (read-line))

(defun menu (string &rest answers)
  (tell string)
  (format t "~{~a~^ ~}~%" answers)
  (loop for answer = (read)
     if (member answer answers :test #'eql)
     return answer))



(defclass talker ()
  (:metaclass findable-class))


(let ((table (make-hash-table :test #'eq)))
  (defun find-talker (talker-name)
    (etypecase talker-name
      (symbol
       (gethash talker-name table))
      (talker
       talker-name)))
  (defun set-talker (name talker)
    (check-type name symbol)
    (check-type talker talker)
    (setf (gethash name table) talker))
  (defun delete-talker (talker-name)
    (remhash (slot-value (find-talker talker-name) 'name)
             table)))

(defgeneric say (who what &optional stream))

(defmethod say ((who talker) what &optional stream)
  (format stream (format nil "~a: ~a~%" who what)))

(defgeneric select (object list))

(defvar *talker*)

(defun talk-form (symbol)
  (case symbol
    (* ((user-select)

(defgeneric ...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((object-expand (object)
             (etypecase object
               (string `((say *talker* ,object)))
               (symbol (talk-form object))
               (list (list object))))
           (body-expand (counter body)
             (when body
               (destructuring-bind (object . body) body
                 `(,@(object-expand object)
                     ,@(talk-expand body)))))
           (talk-expand (body)
             (tagbody
                (mappend 
    (defmacro talk (&body body)
      `(talk-expand body)))
    (defmacro dialog (&body body)
      `(lambda () ,@(talk-expand body)))
    (defmacro define-dialog (name &body body)
      `(defun ,name () ,@(talk-expand body)))))


(defmacro talk ((&key (input-function ''input-test ) ) &body body &aux (cur 0))
  (with-gensyms (state var)
    `(lambda (&optional (,state 1))
       (do ((,var ,state (1+ ,var)))
           (nil)
         (case ,var
           ,@(append
              (mapcar (lambda (expr)
                        (list (incf cur 1)
                              expr))
                      body)
              '((t (return)))))
         (or (funcall ,input-function) (return ,var))))))

(defparameter *example-talk*
  (talk ()
    (say "Heute war so ein schöner Tag. Daher gehe ich zu Lukas")
    (say "Hallo Lukas" "Ich")
    (say "Hallo auch, du" "Lukas")
    (say "Und wie geht es dir?")
    (say "Ja gut, schön, dass du fragst")
    (say "Wir gehen also zusammen spazieren" nil)
    (say "Oha" nil)
    (say "Ja")
    ))

(defun talking (function)
  (let ((pos 1))
    (lambda ()
      (setq pos (funcall function pos))
      (when pos
        (format t "didn't talk until end~%really quit?~%")
        (input-test)))))

(defun test ()
  (let ((fun (talking *example-talk*)))
    (loop while (funcall fun))))

(defvar *active-talk*)

(defvar *talk-position* 1)

(defun call (talk)
  (setq *active-talk* talk)
  (????))

(defun tell ()
  (funcall *active-talk* *talk-position* t))

