(defpackage #:clg-win
  (:use #:cl #:clg-spat #:clg-in #:alexandria #:bordeaux-threads)
  (:export #:window
           ))

(in-package #:clg-win)


(defclass window (glut:window camera cluster) ()
  (:default-initargs :width 1280 :height 768 :resizable t :fps 32
                     :mode '(:double :rgb :multisample :depth)))

(defmethod initialize-instance :after ((win window) &key fps)
  (with-slots (glut::tick-interval) win
    (if fps (setf glut::tick-interval (round (/ 1000 fps))))))

(defmethod glut:reshape ((win window) w h)
    (gl:viewport 0 0 w h) )


(defmethod glut:reshape ((window window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((w (/ width 2))
        (h (/ height 2)))
    (gl:ortho (- w) w h (- h) -65536 65536))
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defmethod glut:display-window :before ((win window))
  (reset-keys)
  (gl:clear-color 1 1 1 0)
  (gl:color 0 0 0)
  (gl:enable :depth-test)
  ;;(gl:enable :blend)
  ;;(gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (with-slots ((w glut:width) (h glut:height)) win
    (let ((w (/ w 2))
          (h (/ h 2)))
      #+nil(gl:frustum (- w) w h (- h) 1024 65536)
      (gl:ortho (- w) w h (- h) -65536 65536)))
  )

(define-modify-macro invertf () not)

(defmethod glut:display :before ((win window))
  #+nil
  (if (invertf flag)
      (progn
        (gl:depth-func :less)
        (gl:depth-range 0.0 0.5))
      (progn
        (gl:depth-func :greater)
        (gl:depth-range 1.0 0.5)))
  #+nil(gl:matrix-mode :modelview)
  (gl:clear :color-buffer :depth-buffer))

(defmethod glut:display :after ((win window))
  (glut:swap-buffers)
  )


(defmethod glut:keyboard ((w window) key x y)
  (press-key key))

(defmethod glut:keyboard-up ((w window) key x y)
  (release-key key))

(defmethod glut:special ((w window) key x y)
  (press-key key))

(defmethod glut:special-up ((w window) key x y)
  (release-key key))

(defmethod glut:close ((w window))
  (clg-sound::reset-mixer))

(defmethod glut:tick ((win window))
  (let ((error (gl:get-error)))
    (unless (eq error :zero) (error error)))
  (act-keys)
  (act win)
  (glut:post-redisplay))

#+nil
(defun package-generic-functions ()
  (dolist (gf (remove-if-not (lambda (symbol) (and (fboundp symbol)
                                                   (typep (symbol-function symbol) 'generic-function)))
                             (apropos-list "" :glut t)))
    (print gf)))


;;(glut:special 

(in-package :cl-user)

#|
(defun to-gl-array (seq type
                    &aux
                      (length (length seq))
                      (array (gl:alloc-gl-array type length)))
  (dotimes (i length)
    (setf (gl:glaref array i) (aref seq i)))
  array)

(defvar *vs*)
(defvar *vs-source*
  "#version 330

attribute vec2 c2d;
void main(void) {
  gl_Position = vec4(c2d, 0.0, 1.0);
}
")

(defvar *fs*)
(defvar *fs-source*
  "#version 330

void main(void) {
  gl_FragColor[0] = 0.0;
  gl_FragColor[1] = 0.0;
  gl_FragColor[2] = 1.0;
}
")

(defvar *triangle*
  (to-gl-array #(0.0 768.0
                 1280.0 768.0
                 640.0 0.0) :float))

;;(defvar *c2d*)

(defvar *program*)

(defun make-shader (type &rest texts)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader texts)
    (gl:compile-shader shader)
    (format t "Shader:~%~a~%" (gl:get-shader-info-log shader))
    shader))

(defun make-program (&rest shaders)
  (let ((program (gl:create-program)))
    (dolist (shader shaders)
      (gl:attach-shader program shader))
    (gl:link-program program)
    (format t "Program:~%~a~%" (gl:get-program-info-log program))
    program))

(defclass test-window (glut:window)
  (vertex-buffer
   index-buffer)
  (:default-initargs :widht 1280 :height 768))

(defun init ()
  (setq *fs* (make-shader :fragment-shader *vs-source*))
  (setq *vs* (make-shader :vertex-shader *fs-source*))
  (print :da)
  (setq *program* (make-program *vs* *fs*))
  (print :initend)
  (setq *c2d* (gl:get-attrib-location *program* "c2d")))

(defmethod glut:display ((w test-window))
  ;;(gl:use-program *program*)
  (print :start)
  (gl:enable-vertex-attrib-array 0 #+nil *c2d*)
  (let (
  (gl:bind-buffer :array-buffer *triangle*)
  (print :before)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer) #+nil *c2d*)
  (print :after)
  (gl:draw-arrays :triangles 0 3)
  (gl:disable-vertex-attrib-array 0 #+nil *c2d*))

(defun free ()
  (gl:delete-program *program*))



(defmethod glut:display :before ((w test-window))
  (gl:clear :color-buffer-bit))

(defmethod glut:display :after ((w test-window))
  (glut:swap-buffers))

#+nil
(defmethod glut:display ((w test-window))
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 #+nil *triangle* (cffi:null-pointer))
  (gl:color 0.5 0.5 0.5)
  (gl:draw-elements :triangles *triangle*)
  (gl:flush)
  (gl:disable-vertex-attrib-array 0))


(defun test ()
  (glut:display-window (make-instance 'test-window)))




#+nil
(defmethod glut:display-window :before ((w test-window))
  (with-slots (vertex-buffer index-buffer) w

    (destructuring-bind (vbuf ibuf) (gl:gen-buffers 2)
      (setf vertex-buffer vbuf
            index-buffer ibuf))

    (gl:bind-buffer :array-buffer vertex-buffer)
    (gl:bind-buffer :array-buffer index-buffer)

    (with-new-gl-array arr :float 9
      (set-gl-array arr (v* #(-0.5 -0.5 0.0
                              -0.5 0.5 0.0
                              0.5 -0.5 0.0) 1))
      (gl:buffer-data :array-buffer :static-draw arr))

    (with-new-gl-array arr :unsigned-short 6
      (set-gl-array arr #(0 1 2))
      (gl:buffer-data :element-array-buffer :static-draw arr)))
  (gl:clear-color 1 0 1 0))


|#






