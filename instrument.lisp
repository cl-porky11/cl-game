(defpackage #:instrument
  (:use #:cl #:mixalot))

(in-package #:instrument)

(declaim (type (real 0 1) *volume*))
(defparameter *volume* 0)


(defun make-function-streamer (function)
  (lambda (streamer mixer buffer offset length time)
    (declare (ignore streamer mixer time))
    (loop for index upfrom offset repeat length
       do (stereo-incf (aref buffer index)
                       (mono->stereo (round (* *volume* #.(1- (expt 2 15)) (funcall function))))))))

(let ((mixer nil))
  (defun start-sound (&rest streamers)
      (if mixer (destroy-mixer mixer))
      (setq mixer (create-mixer))
      (dolist (streamer streamers)
        (mixer-add-streamer mixer streamer)))
  (defun stop-sound ()
    (if mixer (destroy-mixer mixer))))

(defparameter *freq* 32)

(defun sinus-sound-function (&aux (phase 0))
  (lambda ()
    (incf phase (/ pi *freq*))
    (sin phase)))

(defmacro setn (x value h)
  `(setf ,x (+ (* ,x ,(- 1 h)) (* ,value ,h))))
    

(defun some-cool-function ()
  (let ((phase 0)
        (phase0 0))
    (lambda ()
      (incf phase0 (/ pi (expt 2 12)))
      (incf phase (* (/ pi *freq*) (1+ (/ (cos phase0) 12))))
      (sin phase))))

(defun some-cool-function ()
  (let ((phase 0)
        (phase0 0))
    (lambda ()
      (incf phase0 (/ pi *freq*))
      (incf phase (* (/ pi *freq*) (1+ (/ (cos phase0) 2))))
      (sin phase))))



(defclass window (glut:window) ())

(defvar *width*)
(defvar *height*)

(defmethod glut:reshape ((w window) width height)
    (setq *width* width)
    (setq *height* height))

(defmethod glut:display-window :before ((w window))
  (gl:clear-color 1 1 1 0))

(defmethod glut:display :before ((w window))
  (gl:clear :color-buffer)
  (gl:ortho -1 1 -1 1 -1 1))

(defparameter *angle* 0)

(defmethod glut:display ((w window))
  (let ((vol (sqrt *volume*)))
    (gl:with-primitive :triangle-fan
      (gl:color 0 0 0)
      (gl:vertex 0 0)
      (dotimes (i 257)
        (let ((f (/ i 256)))
          (gl:color (- 1 f) (- 1 f) f)
          (gl:vertex (* vol (sin (+ *angle* (* pi 2 f)))) (* vol (cos (+ *angle* (* pi 2 f))))))))
    (gl:with-primitive :triangles
      (dotimes (i 256)
        (let ((f1 (/ i 256))
              (f2 (/ (1+ i) 256)))
          (gl:color 1 1 1)
          (gl:vertex (sin (+ *angle* (* pi 2 f1))) (cos (+ *angle* (* pi 2 f1))))
          (gl:vertex (sin (+ *angle* (* pi 2 f2))) (cos (+ *angle* (* pi 2 f2))))
          (gl:color (- 1 f1) (- 1 f1) f1)
          (gl:vertex (* vol (sin (+ *angle* (* pi 2 f1)))) (* vol (cos (+ *angle* (* pi 2 f1)))))
          (gl:vertex (* vol (sin (+ *angle* (* pi 2 f1)))) (* vol (cos (+ *angle* (* pi 2 f1)))))
          (gl:color (- 1 f2) (- 1 f2) f2)
          (gl:vertex (* vol (sin (+ *angle* (* pi 2 f2)))) (* vol (cos (+ *angle* (* pi 2 f2)))))
          (gl:color 1 1 1)
          (gl:vertex (sin (+ *angle* (* pi 2 f2))) (cos (+ *angle* (* pi 2 f2)))))))))


(defmethod glut:display :after ((w window))
  (glut:swap-buffers))

(defmethod glut:keyboard ((w window) key x y)
  (case key
    (#\Escape (glut:destroy-current-window))))

(defmethod glut:mouse ((w window) button state x y)
  (if (eq state :up)
      (setn *volume* 0 1))
  (glut:post-redisplay))



(defmethod glut:motion ((w window) x y)
  (let ((x (1- (* 2 (/ x *width*))))
        (y (1- (* 2 (/ y *height*)))))
    (setn *volume* (min 1.0 (+ (expt x 2) (expt y 2))) 1)
    (let* ((angle (mod (+ (atan y x) (/ pi 2)) (* 2 pi)))
           (diff (mod (- angle *angle*) (* 2 pi))))
      (setn *angle* (+ *angle* (- diff pi)) 1))
    (setq *freq* (expt 2 (/ *angle* (* 2 pi)))))
  (glut:post-redisplay))


(defun start ()
  (start-sound (make-function-streamer (some-cool-function)))
  (glut:display-window (make-instance 'window :width 768 :height 768 :resizable t))
  (stop-sound))




