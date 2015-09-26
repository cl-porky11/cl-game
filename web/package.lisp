#+nil
(ql:quickload '(:usocket :bordeaux-threads))

(cl:defpackage #:clg-web
  (:use #:cl #:usocket #:bordeaux-threads #:clg-crypt #:clg-util #:alexandria))
(in-package :clg-web)

(defvar *default-port* 4432)

