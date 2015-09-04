(defpackage #:clg-in
  (:use #:cl #:clg-util #:alexandria)
  (:export #:press-key
           #:release-key
           #:act-keys
           
           #:reset-keys
           
           #:press-action
           #:release-action
           #:hold-action
           
           #:reset-press-action
           #:reset-release-action
           #:reset-hold-action
           
           #:remove-press-action
           #:remove-release-action
           #:remove-hold-action
           
           #:call-press-action
           #:call-release-action
           #:call-hold-action
           
           #:define-press-action
           #:define-release-action
           #:define-hold-action

           #:add-key
           #:remove-key
           #:check-key
           ))

(in-package #:clg-in)


(defvar *key-press-actions* (make-hash-table))
(defvar *key-release-actions* (make-hash-table))
(defvar *key-hold-actions* (make-hash-table))

(defvar *keys* nil)

(defun reset-keys ()
  (setq *keys* nil))


(defun add-key (key)
  (push key *keys*))

(defun remove-key (key)
  (deletef *keys* key))

(defun check-key (key)
  (member key *keys*))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (macrolet ((define-actions (name)
               `(let ((actions (make-hash-table)))
                  (defaccessor ,(intern (format nil "~a-ACTION" name)) (key)
                    (gethash key actions))
                  (defun ,(intern (format nil "RESET-~a-ACTIONS" name)) ()
                    (setf actions (make-hash-table)))
                  (defun ,(intern (format nil "REMOVE-~a-ACTION" name)) (key)
                    (remhash key actions))
                  (defun ,(intern (format nil "CALL-~a-ACTION" name)) (key)
                    (funcall-if (,(intern (format nil "~a-ACTION" name)) key)))
                  (defmacro ,(intern (format nil "DEFINE-~a-ACTION" name)) (key &body body)
                    `(eval-when (:load-toplevel :compile-toplevel :execute)
                         (setf (,',(intern (format nil "~a-ACTION" name)) ',,'key) (lambda () ,@body))))))
             (define-actions* (&rest names)
               `(progn
                  ,@(loop for name in names
                       collect `(define-actions ,name)))))
    (define-actions* press release hold)))

(defun press-key (key)
  (add-key key)
  (call-press-action key))

(defun release-key (key)
  (remove-key key)
  (call-release-action key))

(defun act-keys ()
  (dolist (key *keys*)
    (call-hold-action key)))
