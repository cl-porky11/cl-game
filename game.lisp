(defpackage #:cl-game
  (:nicknames #:clg)
  (:use #:cl #:clg-util #:clg-vec #:clg-rot #:clg-act #:clg-spat #:clg-win #:clg-in #:alexandria #:bordeaux-threads)
  (:export #:start
           #:example
           ))
(in-package #:clg)

#|
test this game with one of these expressions:
  (start 'init-window 'init-planet)
  (start 'init-window 'init-cam 'init-flyballs)

only start is external in this package

|#

(defclass test-ball (positional mover ball
                                scaled colored angled
                                rotator soft smooth
                                breaker physical
                                actor last-collision)
  ()
  (:default-initargs :friction 63/64))

(defvar *example-object*)

(defun menu (question &rest answers)
  (format t question answers)
  (read))


(defun acc-action (other acc)
  (lambda (self &aux (vec (v* (unitvec (current-vector self other)) acc)))
    (accelerate self vec)))

(defun boost-action (other fac &optional (dis 256))
  (lambda (self &aux (rot (v* (unitvec (rotation-to-vector (pos self) (pos other))) 2 pi fac)))
    (with-slots (last-collider) self
      (if (< (current-distance self other) dis)
          (boost self rot)))))

(reset-hold-actions)
(reset-press-actions)
(reset-release-actions)
(define-press-action #\r)
(define-press-action #\Escape
  (glut:leave-main-loop))
  
(defun init-cam (next)
  (let ((cam (make-instance '(camera cluster angled rotator positional mover breaker)
                            :pos (vector 0 0 -1024)
                            :friction 31/32
                            :object next)))
    (let ((fac 4))
      (flet ((v (x y z)
               (rotate-vector (vector x y z) (v- (rot cam)))))
        (define-hold-action #\d
          (accelerate cam (v (- fac) 0 0)))

        (define-hold-action #\s
          (accelerate cam (v 0 (- fac) 0)))

        (define-hold-action #\w
          (accelerate cam (v 0 fac 0)))

        (define-hold-action #\a
          (accelerate cam (v fac 0 0)))

        (define-hold-action #\Space
          (accelerate cam (v 0 0 (+ fac))))))

    
    (let ((fac (/ pi 1024)))
      (define-hold-action :key-left
        (boost cam (make-rotation :axis #(0 -1 0)
                                  :angle fac)))
      (define-hold-action :key-right
        (boost cam (make-rotation :axis #(0 1 0)
                                  :angle fac)))
      (define-hold-action :key-down
        (boost cam (make-rotation :axis #(-1 0 0)
                                  :angle fac)))
      (define-hold-action :key-up
        (boost cam (make-rotation :axis #(1 0 0)
                                  :angle fac))))
    cam))

(defun init-planet ()
  (let ((size 768))
    (let* ((object (make-instance 'test-ball ;'(positional mover angled rotator colored scaled ball)
                                  :hardness 1/2
                                  :pos (vector 0 0 size) :size 16
                                  :mass 1
                                  :color '(1 0 0)))
           #+nil
           (planet (make-instance '(positional ball collider physical colored)
                                  :pos (vector 0 0 0)
                                  :color '(0 0 1)
                                  :size size
                                  :mass (expt size 3)))
           (planet (make-instance 'test-ball
                                  :hardness 1/16
                                  :roughness 1
                                  :size size
                                  :mass (expt size 3)
                                  :spin (make-rotation :axis #(1 1 16) :angle (/ pi 32))
                                  :color '(0 0 1 1/2)))
           (rest (append
                  (mappend (lambda (arg)
                             (let* ((ang (* (/ pi 4) arg))
                                    (cos (cos ang))
                                    (sin (sin ang)))
                               (list
                                (make-instance 'test-ball
                                               :action (boost-action object 1/64 512)
                                               :color '(0 1 1)
                                               :mass 16
                                               :size 64
                                               :roughness 1/4
                                               :pos (vector (* size cos) (* size sin) 0))
                                (make-instance 'test-ball
                                               :action (boost-action object -1/16)
                                               :color '(0 1 0)
                                               :mass 1
                                               :size 16
                                               :pos (vector (* cos size 1/4) (* sin size 1/4) size)
                                               #+nil :spin #+nil(make-rotation :angle (* pi 1/4)
                                                                    :axis (vector sin (- cos) 0))))))
                           (iota 8))
                  (mappend (lambda (arg)
                             (let* ((ang (* (/ pi 4) arg))
                                    (cos (cos ang))
                                    (sin (sin ang))
                                    (pos (v* (vector cos sin 2) (/ size 2)))
                                    (list nil))
                               (map nil
                                    (lambda (arg)
                                      (let* ((ang (* (/ pi 2) arg))
                                             (cos (cos ang))
                                             (sin (sin ang))
                                             (pos (v+ pos (v* (vector cos sin 0) 64))))
                                        (push
                                         (make-instance 'test-ball
                                                        :mass 1/4
                                                        :color '(1 1 0)
                                                        :size 16
                                                        :pos pos)
                                         list)))
                                    (iota 4))
                               (loop for (ball . rest) on list
                                  do (dolist (b rest)
                                       (push (make-instance '(colored connection #+nil clg-spat::hard)
                                                            :color '(0 0 0)
                                                            :pos0 ball
                                                            :pos1 b
                                                            :length 128
                                                            :strength 1/8)
                                             list)))
                               list))
                           (iota 8)))))
      (let ((ins
             (make-instance '(gravity-cluster interact-cluster looking)
                            :gravity (/ size)
                            :look-object object
                            :focus-object planet
                            :object (list* planet object rest))))
        (let ((fac 4))
          (flet ((vec (x y z)
                   (rotate-vector (vector x y z)
                                  #+nil
                                  (rot ins)
                                  (rotation-to-vector #(0 0 1) (current-vector planet object)))))
            (define-hold-action (#\a :key-left)
              (boost object (make-rotation :axis (vec 0 -1 0)
                                           :angle (/ fac (size object)))))
            (define-hold-action (#\w :key-up)
              (boost object (make-rotation :axis (vec 1 0 0)
                                           :angle (/ fac (size object)))))
            (define-hold-action (#\s :key-down)
              (boost object (make-rotation :axis (vec -1 0 0)
                                           :angle (/ fac (size object)))))
            (define-hold-action (#\d :key-right)
              (boost object (make-rotation :axis (vec 0 1 0)
                                           :angle (/ fac (size object)))))))
        ins))))


(defun init-flyballs ()
  ;(gl:ortho -384 384 -384 384 -65536 65536)
  (gl:frustum -1 1 1 -1 1024 65536)
  (make-instance '(solid-cluster camera)
                 :object
                 (flet ((rand () (- (random 8192) 4096)))
                   (mapcar (lambda (arg)
                             (declare (ignore arg))
                             (make-instance '(positional ball colored angled rotator)
                                            :color (list (random 1.0) (random 1.0) (random 1.0))
                                            :size (* 16 (1+ (random 16)))
                                            :spin (make-rotation :axis #(0 1 0)
                                                                 :angle (- (random (/ pi 8)) (/ pi 16)))
                                            :pos (vector (rand) (rand) (rand))))
                           (iota 256)))))

(defun init-tex ()
  (make-instance '(angled rotator breaker textured)
                 :friction 255/256
                 :spin (make-rotation :angle (/ pi 8)
                                      :axis #(0 1 0))
                 :image-file "lisplogo_fancy_256.png"))

(defun init-window (next)
  (make-instance 'window :fps 32
                 :width 768
                 :object next
                 ))

(defun start (&rest init-funs)
  (glut:display-window (funcall (apply #'compose init-funs))))

(defun example ()
  (start 'init-window 'init-tex))

#+nil
(defmethod act progn ((ball test-ball))
  (with-slots ((freq clg-spat::frequency) spin size) ball
    (if freq
        (setf freq (+ (* freq (/ (1- size) size)) (/ (rotation-angle spin)))))))



#|
#|
  #+nil
    (:balls
     (dotimes (i 16)
       (let ((j (+ (* i 32) 64)))
         (make-instance 'test-ball
                        :color (list (random 1.0) (random 1.0) (random 1.0))
                        :pos (vector (* j j (/ 256)) j)
                        :vel (let ((r (if (oddp i) 1 -1))) (vector (/ r (1+  i)) (/ (* 2/3 r) (- 16 i))))
                        :scale 16))))
    #+nil
    (:line
     (let ((ball (create 'test-ball :pos (vector 0 -256)
                                   :vel (vector 0 0)
                                   :scale 16)))
       (define-hold-action #\a
         (accelerate ball #(-2 0)))
       (define-hold-action #\d
         (accelerate ball #(2 0)))
       (define-press-action #\w
         (mulf (scale ball) 2))
       (define-release-action #\w
         (mulf (scale ball) 1/2)))
     (let (last current)
       (setq last (make-instance 'positional :pos (vector -512 0)))
       (dotimes (i 15)
         (if t; (evenp i)
           (make-instance 'test-ball :pos (vector (+ -512 (* 64 (1+ i))) -16)
                              :vel (vector 0 0)
                              :scale 16
                              :color (list (random 1.0) (random 1.0) (random 1.0))))
         (setq current (make-instance 'mover :pos (vector (+ -512 (* 64 (1+ i))) 0)
                                      :vel (vector 0 0)))
         (create 'bridge :posa last :posd current :length 32 :strength 1/4)
         (setq last current))
       (setq current (create 'positional :pos (vector 512 0)))
       (create 'bridge :posa last :posd current :length 32 :strength 1/4)
       (create 'accelerator :acc #(0 1/2))
       (create 're :re 63/64)))
    #+nil
    (:jump
     (create 'accelerator :acc (vector 0 1/4))
     (create 're :re 31/32)
     (create 'test-ball
             :color (list (random 1.0) (random 1.0) (random 1.0))
             :pos (vector 0 -64)
             :vel (vector 0 0)
             :scale 16)
     (let ((dl (create 'positional :pos (vector -32 32)))
           (dr (create 'positional :pos (vector 32 32)))
           (ur (create 'mover :pos (vector 32 -32) :vel (vector 0 0)))
           (ul (create 'mover :pos (vector -32 -32) :vel (vector 0 0)))
           left right)
       (setq left (create 'bridge :posa dl :posd ul :length 64 :strength 1/8))
       (setq right (create 'bridge :posa dr :posd ur :length 64 :strength 1/8))
       (define-press-action #\space
         (mulf (slot-value left 'length) 2/3)
         (mulf (slot-value right 'length) 2/3))
       (define-release-action #\space
         (mulf (slot-value left 'length) 3/2)
         (mulf (slot-value right 'length) 3/2))
       (create 'bridge :posa dl :posd ur :length 96 :strength 1/8)
       (create 'bridge :posa dr :posd ul :length 96 :strength 1/8)
       (create 'bridge :posa ul :posd ur :length 64 :strength 1/8)))
    #+nil
    (:snake
     (let (last current)
       (setq last (create 'test-ball :pos (vector -512 0)))
       (dotimes (i 15)
         (setq current (create 'test-ball :pos (vector (+ -512 (* 64 (1+ i))) 0)))))) |#
(defun start ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 640 :h 384 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (gl:ortho -640 640 384 -384 -1 1)
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


#|
(defmethod test-init ((type (eql 0)) &rest rest)
  (assert (null rest))
  (let ((fac 1/4))
    (define-hold-action (#\a :key-left)
      (accelerate *example-object* (vector (- fac) 0 0))
      (boost *example-object* (make-rotation :axis #(0 -1 0) :angle (/ fac (size *example-object*)))))
    
    (define-hold-action (#\w :key-up)
      (accelerate *example-object* (vector 0 (- fac) 0))
      (boost *example-object* (make-rotation :axis #(1 0 0) :angle (/ fac (size *example-object*)))))
    
    (define-hold-action (#\s :key-down)
      (accelerate *example-object* (vector 0 fac 0))
      (boost *example-object* (make-rotation :axis #(-1 0 0) :angle (/ fac (size *example-object*)))))
    
    (define-hold-action (#\d :key-right)
      (accelerate *example-object* (vector fac 0 0))
      (boost *example-object* (make-rotation :axis #(0 1 0) :angle (/ fac (size *example-object*))))))
  (cons
   (setq *example-object*
         (make-instance 'test-ball
                        :pos (vector -32 0 16) :vel (vector 0 0 0)
                        :color (list 1 0 0) :size 16
                        :mass (* 4 4 4)
                        :rot (vector 1 0 0)
                        ))
   (mapcar (lambda (arg)
             (make-instance 'test-ball
                            :pos (vector (* 4 arg arg) 0 (* 4 arg)) :vel (vector 0 0 0)
                            :color (list 0 1 1) :size (* 4 arg)
                            :mass (* arg arg arg)
                            :rot (vector 1 0 0)
                            ))
           (iota 8 :start 1))))
|#


#+nil
(defun start ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 1280 :h 768 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (gl:ortho 0 1280 768 0 -1 1)
        ;(gl:enable :blend)
        ;(gl:blend-func :src-alpha :one-minus-src-alpha)
        (gl:clear-color 0 0 0 1)
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)
          (:keydown (:keysym key)
            (and (sdl2:scancode= (sdl2:scancode-value key) :scancode-escape)
                 (sdl2:push-event :quit)))
          (:idle ()
            (handler-case 
                (progn
                  (gl:clear :color-buffer)
                  (gl:color 0 0 1)
                  (run)
                  (gl:flush)
                  (sdl2:gl-swap-window win))
              (t (sdl2:push-quit-event)))))
          (finish-output)))))

;(defmethod interact ((balla test-ball) (balld test-ball)))



;(loop repeat 256 do (progn (print :running) (sleep 1/32) (run)))

;(start)

;;;rest

#+nil
(defmethod* interact ((world world) (person person))
  (accelerate person (gravity world)))

#+nil
(defclass world-line (line)
  (positionals))

#+nil
(defmethod* interact ((person person) (world-line world-line)))



;;;physical


(defvar *gravity* 0)

(defclass physical (mover)
  ((mass :accessor mass :initarg :vel :type real)))

#+nil
(defclass rotated ()
  (rot :accessor rot :initarg :rot))


#+nil
(defun gravity (movera moverd
                &key
                  (strength 1)
                  (vec (v- (pos movera) (pos moverd)))
                  (distance (absvec vec)))
  (accelerate2 movera moverd (v* vec strength (/ (expt distance 3))) (/ (mass moverd) (mass movera))))

#+nil
(defmethod interact ((physicala physical) (physicald physical))
  (collide physicala physicald)
  (gravity physicala physicald))


#+nil
(defun accelerate-to (mover goal acc)
  (let* ((d (- (pos goal) (pos mover)))
         (dis (absvec d))
         (a (v* d (/ acc dis))))
    (accelerate mover a)))






;;;actors

(defclass actor ()
  ((action :accessor action :initarg :action :type (or function null))))

(defmethod act :before ((actor actor))
  (if (action actor)
    (funcall (action actor) actor)))

(defmethod act ((actor actor)))

(defun combine-actions (first second actor)
  (declare (type (or function null) first second) (type actor actor))
  (if first
    (funcall first actor)
    (unless (action actor)
      (setf (action actor) second))))

(defun act-before (actor function)
  (setf (action actor) (lambda (entity) (combine-actions function (action actor) entity))))

(defun act-after (actor function)
  (setf (action actor) (lambda (entity) (combine-actions (action actor) function entity))))

;;;persons

(defclass person (mover actor)
  ((acc :reader acc :initarg :acc :type number)))

(defmethod move-to ((person person) (where positional))
  (act-before person
    (lambda (person)
      (accelerate-to person where (acc person)))))


;;;specialized


;;;test

#|
                   :actor (mesh ((w #(0 0 0))
                                  (x #(1 0 0))
                                  (y #(0 1 0))
                                  (z #(0 0 1)))
                             (w x y) (w y z) (w z x)))))
  (glfw:do-window (:title "Lesson 1" :width 320 :height 240)
      ((glfw:set-key-callback 'key-callback)
       (glfw:set-window-size-callback 'window-size-callback)
       (gl:clear-color 0.0 0.0 0.0 1.0))
    (gl:clear :color-buffer-bit)))

|#
                    

;;;generics*

(defgeneric move-to (who where))

(defgeneric action (actor))

(defmethod act (actor))

|#

