(defpackage #:clg-win
  (:use #:cl #:clg-spat)
  (:export #:window
           #:*width*
           #:*height*
           #:*draw-object*
           #:*act-object*
           #:run
           ))

(defclass window (glut:window) ())

(defvar *width*)
(defvar *height*)

(defvar *draw-object*)
(defvar *act-object*)

(defmethod glut:reshape ((w window) width height)
    (setq *width* width)
    (setq *height* height)
    (gl:ortho 0 *width* *height* 0 -1 1))


(defmethod glut:display-window :before ((w window))
  (gl:clear-color 1 1 1 0))

(defmethod glut:display :before ((w window))
  (gl:clear :color-buffer)
  (gl:ortho 0 *width* *height* 0 -1 1))

(defmethod glut:display ((w window))
  (draw *draw-object*)
  (act *act-object*))


(defmethod glut:display :after ((w window))
  (glut:swap-buffers))

(defmethod glut:keyboard ((w window) key x y)
  #+nil (f key))

#+nil
(defmethod glut:mouse ((w window) button state x y)
  (if (eq state :up)
      (setn *volume* 0 1))
  (glut:post-redisplay))


(defun run (time)
  (loop
     (sleep time)
     (glut:post-redispay)))
