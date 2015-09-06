(defpackage #:clg-win
  (:use #:cl #:clg-spat #:clg-in #:bordeaux-threads)
  (:export #:window
           #:run
           ))

(in-package #:clg-win)


(defclass window (glut:window)
  ((act-object :initarg :act-object)
   (draw-object :initarg :draw-object))
  (:default-initargs :width 1280 :height 768 :resizable t :fps 32 :mode '(:depth)))

(defmethod initialize-instance :after ((win window) &key object fps)
  (with-slots (act-object draw-object glut::tick-interval) win
    (setf act-object object draw-object object)
    (if fps (setf glut::tick-interval (round (/ 1000 fps))))))

(defmethod glut:reshape ((win window) w h)
  (gl:ortho -1 1 -1 1 -1 1))


(defmethod glut:display-window :before ((win window))
  (gl:clear-color 1 1 1 0)
  (gl:enable :depth-test)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (with-slots ((w glut:width) (h glut:height)) win
    (let ((w (/ w 2))
          (h (/ h 2)))
      (gl:ortho (- w) w h (- h) -1024 1024)))
  )

(defmethod glut:display :before ((win window))
  (gl:matrix-mode :modelview)
  (gl:clear :color-buffer :depth-buffer))

(defmethod glut:display ((win window))
  (draw (slot-value win 'draw-object)))

(defmethod glut:display :after ((win window))
  (glut:swap-buffers)
  )


(defmethod glut:keyboard ((w window) key x y)
  (press-key key))

(defmethod glut:keyboard-up ((w window) key x y)
  (release-key key))

(defmethod glut:tick ((win window))
  (act-keys)
  (act (slot-value win 'act-object))
  (glut:post-redisplay))

#+nil
(defun package-generic-functions ()
  (dolist (gf (remove-if-not (lambda (symbol) (and (fboundp symbol)
                                                   (typep (symbol-function symbol) 'generic-function)))
                             (apropos-list "" :glut t)))
    (print gf)))




















