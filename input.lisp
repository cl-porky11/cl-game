(defpackage #:clg-in
  (

(defvar *key-press-actions* (make-hash-table))
(defvar *key-release-actions* (make-hash-table))
(defvar *key-hold-actions* nil)

(defvar *keys* nil)

(defun add-key (key)
  (push key *keys*))

(defun remove-key (key)
  (setf *keys* (delete key *keys*)))

(defun check-key (key)
  (member key *keys*))

(eval-when (:load-toplevel :compile-toplevel)
  (macrolet ((define-actions (name)
               `(let ((actions (make-hash-table)))
                  (defaccessor ,(intern (format nil "~a-ACTION" name)) (key)
                    (gethash key actions))
                  (defun ,(intern (format nil "RESET-~a-ACTIONS" name)) ()
                    (setf actions (make-hash-table)))
                  (defun ,(intern (format nil "REMOVE-~a-ACTION" name)) (key)
                    (remhash key actions))
                  (defmacro ,(intern (format nil "DEFINE-~a-ACTION" name)) (key &body body)
                    `(setf (,',(intern (format nil "~a-ACTION" name)) ',,'key) (lambda () ,@body)))))
             (define-actions* (&rest names)
               `(progn
                  ,@(loop for name in names
                       collect `(define-actions ,name)))))
    (define-actions* press release hold)))
