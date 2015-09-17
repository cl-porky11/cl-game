
(defpackage #:test
  (:use #:cl #:trivial-shell #:ltk))

(in-package #:test)

(defvar *draw-program* "gimp")

(defun open-draw-program (&optional (filename ""))
  (shell-command (format nil "~a ~a" *draw-programm* filename)))

(defun main-menu ()
  (with-ltk ()
    (let ((b (make-instance 'button
                            :master nil
                            :text "Press Me"
                            :command (lambda ()
                                       (format t "Hello World!~&")))))
      (pack b))))




