(defpackage #:cl-game-opengl
  (:nicknames #:clg-gl)
  (:use #:cl #:clg-vec #:clg-rot #:alexandria)
  (:export #:with-transformation
           #:rotate-with-rotation
           #:draw-line
           #:draw-circle
           #:draw-circle*
           #:draw-primitive
           #:draw-primitives
           #:make-gl-array
           ))

(in-package #:clg-gl)

(defmacro with-new-gl-array (var type count &body body)
  `(let ((,var (gl:alloc-gl-array ,type ,count)))
     ,@body
     (gl:free-gl-array ,var)))

(defun set-gl-array (glarr arr)
  (dotimes (i (length arr))
    (setf (gl:glaref glarr i) (aref arr i))))

(defun make-gl-array (type array)
  (let ((glarr (gl:alloc-gl-array type (length array))))
    (set-gl-array glarr array)
    glarr))

(defparameter *circle-quality* 16)

#+nil
(gl:define-gl-array-format position2d-color
  (gl:vertex :type :float :components (x y))
  (gl:color :type :unsigned-char :components (r g b)))
             

#+nil
(defun make-circle (&optional (quality *circle-quality*))
  (let ((indices-array
         (make-gl-array :unsigned-short (coerce (list* quality (1- quality) (iota quality)) 'vector)))
        (vertex-array (gl:alloc-gl-array 'position2d-color quality)))
    (dotimes (i quality)
      (let ((rot (* 2 pi (/ i quality))))
        (setf (gl:glaref vertex-array i 'x) (coerce (sin rot) 'single-float)
              (gl:glaref vertex-array i 'y) (coerce (cos rot) 'single-float)
              (gl:glaref vertex-array i 'r) 0
              (gl:glaref vertex-array i 'g) 0
              (gl:glaref vertex-array i 'b) 0)))
    (setf (gl:glaref vertex-array quality 'x) 0.0
          (gl:glaref vertex-array quality 'y) 0.0
          (gl:glaref vertex-array quality 'r) 255
          (gl:glaref vertex-array quality 'g) 255
          (gl:glaref vertex-array quality 'b) 255)
    (list indices-array vertex-array)))

(let ((last 0) circle)
  (defun make-circle (&optional (quality *circle-quality*))
    (if (= last quality)
        circle
        (setq last quality
              circle
              (let (vertices)
                (dotimes (i quality)
                  (let ((rot (* 2 pi (/ i quality))))
                    (push (vector (cos rot) (sin rot))
                          vertices)))
                vertices)))))

#+nil
(defparameter *circle* (make-circle *circle-quality*))

#+nil
(defun draw-circle* (&optional circle)
  (destructuring-bind (indices vertices) circle
    (gl:bind-gl-vertex-array vertices)
    (gl:draw-elements :triangle-fan indices)
    ))
                     

(defun draw-circle (&optional (quality *circle-quality*))
  (gl:with-pushed-attrib (:current-bit)
    (gl:with-primitive :triangle-fan
      (gl:vertex 0 0)
      (gl:color 0 0 0)
      (let ((c (make-circle quality)))
        (dolist (v c)
          (gl:vertex (aref v 0) (aref v 1)))
        (gl:vertex (aref (car c)  0) (aref (car c) 1))))))

(defun rotate-with-rotation (rot)
  (gl:rotate (/ (* 180 (rotation-angle rot) ) pi)
             (aref rot 0)
             (aref rot 1)
             (aref rot 2)))

(defun draw-line (posa posd)
  (let ((< (>= (length posa) 3)))
    (gl:with-primitive :lines
      (gl:vertex (aref posa 0)
                 (aref posa 1)
                 (if < (aref posa 2) 0))
      (gl:vertex (aref posd 0)
                 (aref posd 1)
                 (if < (aref posd 2) 0)))))


#+nil
(defun draw-things (type &rest vertices)
  (gl:bind-buffer :array-buffer (apply #'concatenate 'vector vertices))
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
  (gl:draw-arrays type 0 3)
  (gl:disable-vertex-attrib-array 0))

(defmacro with-transformation ((&key translate
                                     scale
                                     rotate)
                               &body body
                               &aux (scale-sym (gensym "SCALE")))
  `(gl:with-pushed-matrix
     ,(if translate `(gl:translate ,@translate))
     ,(if scale
          `(typecase scale
             (list
               (gl:scale ,@scale))
             (integer
               (let ((,scale-sym ,scale))
                 (gl:scale ,scale-sym ,scale-sym ,scale-sym)))))
     ,(if rotate
          `(typecase rotate
                     (integer `(gl:rotate rotate 0 0 1))))
     ,@body))


(defmacro draw-primitive (primitive &body args)
  `(gl:with-primitive ,primitive
     ,@(mapcar (lambda (args) `(gl:vertex ,@args)) args)))

(defmacro draw-primitives (primitive &body args)
  `(draw-primitive ,primitive ,@args))


#+nil
(defun start ()
  ;;(init)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 1280 :h 768 :flags '(:shown :opengl :resizable))
      (sdl2:with-gl-context (gl-context win)
        (gl:ortho 0 1280 768 0 -1 1)
        (gl:clear-color 0 0 0 1)
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)
          (:windowevent (:data1 w :data2 h)
            (sdl2:set-window-size win w h))
          (:keydown (:keysym key)
            (and (sdl2:scancode= (sdl2:scancode-value key) :scancode-escape)
                 (sdl2:push-event :quit)))
          (:idle ()
            (gl:clear :color-buffer)
            (gl:color 0 0 1)
            (run)
            (gl:flush)
            (sdl2:gl-swap-window win))
          (finish-output)))))
  (free))
