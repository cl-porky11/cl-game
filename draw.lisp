(defpackage #:draw
  (:use #:cl #:alexandria #:clg-gl #:clg-util))

(in-package #:draw)




;;;primitive

(defclass primitive ()
  ((type :initarg :type)
   (vertices :initarg :vertices))
  (:default-initargs :type :quads :vertices '((0 0) (0 1) (1 1) (1 0))))

(defmethod draw ((primitive primitive))
  (with-slots (type vertices) primitive
    (with-primitive type
      (dolist (vertex vertices)
        (apply #'gl:vertex vertex)))))

(defclass primitives ()
  ((primitives :initform nil)))

(defmethod draw ((primitives primitives))
  (dolist (primitives primitives)
    (draw primitive)))

(defvar *line-color* '(1 1 1))
(defvar *text-color* '(1 1 1))
(defvar *fill-color* '(0 0 0))

(defclass box (positional stretched)
  ((line-color :accessor line-color :initarg :line-color :initform *line-color*)
   (fill-color :accessor fill-color :initarg :fill-color :initform *fill-color*)
   ))

(defmethod draw ((box box))
  (with-slots (width height line-color fill-color) box 
    (apply #'gl:color line-color)
    (draw-primitives :line-loop
      (0 0)
      (0 height)
      (width height)
      (width 0))
    (apply #'gl:color fill-color)
    (draw-primitives :quads
      (0 0)
      (0 height)
      (width height)
      (width 0))
    ))

(defclass text-box (box)
  ((text :accessor box-text :initarg :text :initform "")
   (text-color :accessor text-color :initarg :text-color :initform *text-color*)))

(defparameter *markables* nil)

(defclass markable ()
  ((marked :initform (progn (

(defgeneric mark (markable x y))

(defmethod 

;(defmethod draw :after ((box text-box

;(defmethod 



(defclass window (glut:window)
  ()
  (:default-initargs :width 1280 :height 768 :title ""
;                     :tick-interval 32
                     :mode '(:single :rgb :depth)))
#+nil
(defun start ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 640 :h 384 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        ;;(gl:enable :blend)
        ;;(gl:blend-func :src-alpha :one-minus-src-alpha)
        (gl:clear-color 0 0 0 1)
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)
          (:keydown (:keysym key)
            (with-key-char char key
              (unless (check-key char)
                (add-key char)
                (funcall-if (press-action char)))))
          (:keyup (:keysym key)
            (with-key-char char key
              (when (check-key char)
                (remove-key char)
                (funcall-if (release-action char)))))
          (:idle ()
            (sdl2:delay (round (/ *frame-rate*) 1/1000))
            (gl:clear :color-buffer)
            ;;(gl:color 0 0 0 1)
            ;;(gl:rect -640 -384 640 384)
            (dolist (char *keys*)
              (funcall-if (hold-action char)))
            (run)
            (display)
            (gl:flush)
            (sdl2:gl-swap-window win)))
        (finish-output)))))

(defmethod glut:display-window :before ((w window))
  (gl:ortho -640 640 384 -384 -1 1)
  (gl:clear-color 0 0 0 0))

(defmethod glut:display ((window window))
  (gl:clear :color-buffer)
  (draw *draw-objects*))

(defmethod glut:reshape ((window window) width height)
  (gl:ortho (- (/ width 2)) (/ width 2) (- (/ height 2)) (/ height 2) -1 1)
  (gl:viewport 0 0 width height))

(defmethod glut:keyboard ((window window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defmethod glut:mouse ((window window) button state x y))

(defmethod glut:motion ((window window) x y)
  (dolist (markable *markables*)
    (mark markable)))
  

(defun start ()
  (glut:display-window (make-instance 'window)))




