(defpackage #:clg-talk
  (:use #:cl #:alexandria)
  (:export #:say
           ))

(in-package #:vn-talk)

(defvar *say-output* t)

(defparameter *current-person* nil)

(defparameter *previous-person* nil)


(defgeneric generic-say (what who stream)
  (:method (what who stream)
    (format stream "~a: ~a~%" who what)))

(defun set-person (&optional (person *previous-person*))
  (unless (eq person *current-person*)
    (setq *previous-person* *current-person*
          *current-person* person)))

(defun say (what &optional (person *current-person*) (stream *say-output*))
  (unless (eq person *previous-person*)
    (setq *current-person* *previous-person*
          *previous-person* person))
  (generic-say what person stream))

(defclass person () ())

(defgeneric show (person)
  (:method (person)))


(defmethod generic-say (what (who null) stream)
  (format stream "~a~%" what))


(defmethod generic-say :before (what (who person) stream)
  (show who))


(defun input-test () (string/= (read-line) "end"))

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

