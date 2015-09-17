(defpackage #:clg-stories
  (:use #:cl #:clg-util #:alexandria)
  (:export #:define-story
           #:delete-story
           #:start-story
           #:exact-prequels
           #:minimum-prequels
           #:again
           ))
(in-package #:clg-stories)

(defvar *active-stories*)

(defstruct (story (:constructor
                   make-story (name &key enable disable condition activator deactivator selector)))
  name
  condition
  (enable #'cons)
  disable
  activator
  deactivator
  selector)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (let ((table (make-hash-table :test #'eq)))
    (defun find-story (story-name)
      (etypecase story-name
        (symbol
         (values (gethash story-name table) story-name))
        (story
         (values story-name (story-name story-name)))))
    (defun set-story (name story)
      (check-type name symbol)
      (check-type story story)
      (setf (gethash name table) story))
    (defun delete-story (story-name)
      (remhash (story-name (find-story story-name))
               table))))

(defvar *active-stories*)

(defun story-active-p (story-name)
  (member (story-name (find-story story-name)) *active-stories*))

(defun activate-story (story-name)
  (multiple-value-bind (story name) (find-story story-name)
    ;;(print (list :activate name))
    (push name *active-stories*)
    (funcall-if (story-activator story))))

(defun deactivate-story (story-name)
  (multiple-value-bind (story name) (find-story story-name)
    ;;(print (list :deactivate name))
    (deletef *active-stories* name :count 1)
    (funcall-if (story-deactivator story))))

(defvar *finished-table*)

(defun disable-story (story-name)
  (multiple-value-bind (story name) (find-story story-name)
    (setf (gethash name *finished-table*)
          (funcall-if (story-disable story) (gethash name *finished-table*)))))

(defun enable-story (new story-name)
  (multiple-value-bind (story name) (find-story story-name)
    (setf (gethash name *finished-table*) (funcall-if (story-enable story)
                                                      new
                                                      (gethash name *finished-table*)))))


(defun story-callable-p (story-name)
  (multiple-value-bind (story name) (find-story story-name)
    (funcall-if (story-condition story) (gethash name *finished-table*))))

(defun exact-prequels (&rest prequels)
  (let ((prequels (mapcar #'ensure-list prequels)))
    (lambda (finished)
      (member-if (lambda (prequel) (same-elements-p finished prequel :test #'equal))
                 prequels))))

(defun minimum-prequels (&rest prequels)
  (let ((prequels (mapcar #'ensure-list prequels)))
    (lambda (finished)
      (member-if (lambda (prequel) (subsetp prequel finished :test #'equal))
                 prequels))))

(defun again ()
  (lambda (finished)
    finished))

(defun delete-once ()
  (lambda (finished)
    (delete-duplicates-not finished)))


(defun call-story (story-name)
  (multiple-value-bind (story name) (find-story story-name)
    (disable-story story)
    (deletef *active-stories* name :count 1)
    (dolist (next (ensure-list (funcall-if (story-selector story))))
      (enable-story name next)
      (if (symbolp next)
          (let ((active (story-active-p next))
                (callable (story-callable-p next)))
            ;;(print (list name :active active :callable callable))
            (cond
              ((and callable (not active))
               (activate-story next))
              ((and (not callable) active)
               (deactivate-story next))))
          (warn "The selector of the story should only return lists of symbols or symbols~%~\
                 You returned the ~a ~a"
                (type-of next) next)))
    (if (and (story-callable-p story) (not (story-active-p story)))
        (activate-story story))))


(defun ensure-story (name &rest args)
  (set-story name
             (apply #'make-story name args)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((expand-function (function)
             (destructuring-bind (function . args) function
               (etypecase function
                 (symbol `(,function ,@(mapfun ((arg args)) `',arg)))
                 (list (check-type args null)
                       function))))
           (expand-keyword (keyword body)
             (ecase keyword
               (:activator `(lambda () ,@body))
               (:deactivator `(lambda () ,@body))
               (:selector `(lambda () ,@body))
               (:condition (expand-function body))
               (:enable (expand-function body))
               (:disable (expand-function body))))
           (expand-lists (lists)
             (if lists
                 (destructuring-bind ((keyword . body) . lists) lists
                   `(,keyword ,(expand-keyword keyword body) ,@(expand-lists lists))))))
    (defmacro define-story (name &rest lists)
      `(ensure-story ',name ,@(expand-lists lists)))))

(defun start-story (&key active (selector #'car)
                    &aux (*finished-table* (make-hash-table :test #'eq))
                      *active-stories*)
  (dolist (active (ensure-list active))
    (activate-story active))
  (loop while *active-stories*
     do (call-story (funcall selector (reverse *active-stories*)))))






