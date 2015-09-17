(defpackage #:subpackages
  (:use :cl)
  (:export #:define-subpackage
           #:find-subpackage
           #:in-subpackage
           #:use-subpackage
           #:unuse-subpackage
           #:list-all-subpackages

           #:add-subpackage
           #:remove-subpackage
           ))
           


(let ((subpackages (make-hash-table :test #'eq)))
  (defaccessor subpackages (&optional (package *package*) &aux (package (find-package package)))
    (gethash package subpackages)))

(defun add-subpackage (subpackage &optional (package *package*)
                       &aux (subpackage (find-package subpackage)))
  (pushnew subpackage (package-subpackages package) :test #'eq))

(defun remove-subpackage (subpackage &optional (package *package*)
                          &aux (subpackage (find-package subpackage)))
  (remove subpackage (package-subpackages package) :test #'eq))

(defun subpackagep (subpackage &optional (package *package*)))

(defmacro define-subpackage (name &rest args)
  `(add-subpackage
    (defpackage ,(format nil "~a:~a" (package-name *package*) name)
      ,@args)))

(defun find-subpackage (name &optional (package *package*)
  (if (packagep name)
      (if (package-name
      (find-package (format nil "~a:~a" package-name 
