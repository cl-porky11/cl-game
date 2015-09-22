(defpackage #:clg-talk
  (:use #:cl #:clg-util #:sb-mop #:alexandria #:replaceable-objects)
  (:export #:say

           #:talker
           #:person


           #:dialog
           #:define-dialog

           #:talk-state

           #:select
           #:pause

           #:*talk-exit*
           #:*talk-back*
           #:*talk-restart*
           ))
(in-package #:clg-talk)

(defclass talk-function ()
  ((text-array :initarg :text-array)
   (get-talk-state :initarg :get-talk-state)
   (set-talk-state :initarg :set-talk-state))
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((talk talk-function) &key function)
  (set-funcallable-instance-function talk function))

(defun talk-state (fun)
  (with-slots (get-talk-state) fun
    (funcall get-talk-state)))

(defun (setf talk-state) (val fun)
  (with-slots (set-talk-state) fun
    (funcall set-talk-state val)))


(defgeneric say (who what))

(defmethod say ((who symbol) what)
  (say (or (find-talker who) (error "class not defined")) what))

(defclass talker () ())

(defmethod say ((who talker) what)
  (format t (format nil "~a~%" what)))

(defclass person (talker)
  ((person-name :initarg :called)))

(defmethod say ((who person) what)
  (format t (format nil "~a: ~a~%" (slot-value who 'person-name) what)))

(defvar *talk-exit* '#:exit)
(defvar *talk-back* '#:back)
(defvar *talk-restart* '#:restart)

(defun pause (&aux (in (read-line)))
  (and (<= 4 (length in))
       (or
        (and (string= "exit" in :end2 4)
             *talk-exit*)
        (and (string= "back" in :end2 4)
             *talk-back*)
        (and (<= 7 (length in))
             (string= "restart" in :end2 7)
             *talk-restart*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((string-expand (string)
             (replaceable-form string))
           (user-expand ()
             (declare (special *num* *form*))
             `(let ((input ,*form*))
                (cond
                  ((eq input *talk-back*) ,(if (= *num* 1) '(return) `(go ,(1- *num*))))
                  ((eq input *talk-exit*) (return))
                  ((eq input *talk-restart*) (go 0))
                  (t input))))
           (object-expand (state object)
             (declare (special *talker* *num* *form*))
             (etypecase object
               (string `(,(incf *num*)
                          (setq ,state ,*num*)
                          (say ',*talker* ,(string-expand object))
                          ,(user-expand)))
               (symbol (setq *talker* object) nil)
               (list (setq *form* object) nil)))
           (body-expand (state body)
             (declare (special *num*))
             (destructuring-bind (object . body) body
               (if body
                   `(,@(object-expand state object)
                       ,@(body-expand state body))
                   (let ((body (object-expand state object)))
                     `(,@(butlast body)
                         (return
                           (multiple-value-prog1
                               ,@(last body)
                             (setq ,state nil))))))))
           (talk-expand (state body)
             (let (*talker* (*num* 0) *form*)
               (declare (special *talker* *num* *form*))
               (let ((new-body (body-expand state body)))
                 `(flet ((talk-state () ,state)
                         ((setf talk-state) (value) (setf ,state value)))
                    (declare (ignorable #'talk-state #'(setf talk-state)))
                    (block nil
                      (tagbody
                       0
                         (case ,state
                           ,@(mapcar (lambda (state) `(,state (go ,state)))
                                     (iota *num* :start 1)))
                         ,@new-body))))))
           (function-expand (list body)
             (let (( *replaceable-objects* (make-array 3 :fill-pointer 0 :adjustable t)))
               (with-gensyms (state)
                 `(let (,state)
                    (make-instance 'talk-function
                                   :function (lambda ,list ,(talk-expand state body))
                                   :set-talk-state (lambda (state) (setq ,state state))
                                   :get-talk-state (lambda () ,state)
                                   :text-array *replaceable-objects*))))))
    (defmacro dialog (list &body body)
      (function-expand list body))
    (defmacro define-dialog (name list &body body)
      `(setf (symbol-function ',name) ,(function-expand list body)))))

(defmethod expand-definer-keyword ((name person) keyword entries)
  (car entries))

(defun rename (person to)
  (setf (slot-value person 'person-name) to))




(defvar *select-function*)

(defun select (list)
  (let ((answers (mapcar #'ensure-car list))
        (results (mapcar (lambda (sub) (if (and (listp sub) (cdr sub))
                                           (cadr sub)
                                           (ensure-car sub)))
                         list)))
    (format t "~:{~a: ~a~%~}" (mapfun ((num (iota (length answers) :start 1))
                                       (answer answers)) (list num answer)))
    (loop for answer = (parse-integer (read-line) :junk-allowed t)
       if (and (integerp answer) (<= 1 answer (length list)))
       return (nth (1- answer) results))))


#+nil
(defun select (list)
  (funcall *select-function* list))

(defun menu (person string &rest answers)
  (say person string)
  (select answers))
#+nil
(defun ask (string)
  (tell string)
  (read-line))










#+nil
(defparameter *example-talk*
  (talk ()
        "Heute war so ein schöner Tag. Daher gehe ich zu Lukas"
        ich"Hallo Lukas"
        lukas"Hallo auch, du"
        ich"Und wie geht es dir?"
        lukas"Ja gut, schön, dass du fragst"
        nil"Wir gehen also zusammen spazieren"
        "Oha"
        ich"Ja"
    ))
#+nil
(defun talking (function)
  (let ((pos 1))
    (lambda ()
      (setq pos (funcall function pos))
      (when pos
        (format t "didn't talk until end~%really quit?~%")
        (input-test)))))

#+nil
(defun test ()
  (let ((fun (talking *example-talk*)))
    (loop while (funcall fun))))

(defvar *active-talk*)

(defvar *talk-position* 1)

#+nil
(defun call (talk)
  (setq *active-talk* talk)
  (????))

