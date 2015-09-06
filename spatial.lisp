(defpackage #:clg-spat
  (:use #:cl #:clg-util #:clg-vec #:clg-gl #:alexandria #:quat)
  (:export #:draw
           #:act
           #:interact
           
           #:move
           #:positional
           #:pos
           #:pos-

           #:accelerate
           #:accelerate2
           #:mover
           #:vel
           #:vel-

           #:spin
           #:angled
           #:ang

           #:boost
           #:boost2
           #:rotator
           #:rot

           #:collide
           #:single-touch-distance
           #:touch-distance
           #:current-vector
           #:collide-factor
           #:current-distance
           #:check-collision
           #:collider

           #:repel
           #:repel-vector
           #:repel-factor
           #:distance-vector
           #:edge-vector
           #:repeller

           #:soft
           #:hardness

           #:roll
           #:roll-angle
           #:roll-vector
           #:roll-factor
           #:roller

           #:smooth
           #:roughness

           #:physical
           #:mass

           #:scaled
           #:size

           #:ball
           #:colored
           ))

(in-package #:clg-spat)


(defgeneric draw (object)
  (:method (object)))

(defgeneric act (object)
  (:method (object)))

(defgeneric interact (objecta objectd)
  (:method-combination progn)
  (:method progn (a d)))


;;;default-methods

(defmethod draw ((objects list))
  (dolist (object objects)
    (draw object)))

(defmethod act ((objects list))
  (loop for (object . rest) on objects
     do
       (interact object rest)
     do
       (act object)))

(defmethod interact progn ((actora list) actord)
  (dolist (actor actora)
    (interact actor actord)))

(defmethod interact progn (actora (actord list))
  (dolist (actor actord)
    (interact actora actor)))


;;;classes

(defclass transformed () ())

(defmethod draw :around ((some transformed))
  (gl:with-pushed-matrix
    (call-next-method)))


;;;positions

(defgeneric move (pos vector))

(defclass positional (transformed)
  ((pos :accessor pos
        :initarg :pos
        :type (vector real))))

(defaccessor pos- (positional number)
  (aref (pos positional) number))

(defmethod draw :before ((pos positional))
  (gl:translate (pos- pos 0) (pos- pos 1) (pos- pos 2)))

(defmethod move ((pos positional) vector)
  (incv (pos pos) vector))


;;;movement

(defgeneric accelerate (pos vector))

(defun accelerate2 (movera moverd acc &optional (faca 1) (facd (/ faca)))
  (accelerate movera (v* acc faca))
  (accelerate moverd (v- (v* acc facd))))

(defclass mover ()
  ((vel :accessor vel
        :initarg :vel
        :type (vector real))))

(defaccessor vel- (positional number)
  (aref (vel positional) number))

(defmethod act :after ((mover mover))
  (move mover (vel mover)))

(defmethod accelerate ((mover mover) vector)
  (incv (vel mover) vector))



;;;angeled

(defgeneric spin (object vector))

(defclass angled (transformed)
  ((ang :accessor ang)))

(defun ensure-quaternion (quat)
  (etypecase quat
    (null (quaternion-from-axis-angle #(0 0 1) 0))
    (number (quaternion-from-axis-angle #(0 0 1) quat))
    ((cons number (cons sequence null)) (quaternion-from-axis-angle (cadr quat) (car quat)))
    ((cons number sequence) (quaternion-from-axis-angle (cdr quat) (car quat)))
    (quaternion quat)))

(defmethod initialize-instance :after ((ins angled) &key ang)
  (setf (slot-value ins 'ang)
        (ensure-quaternion ang)))

(defmethod draw :before ((rot angled))
  (bind-quaternion (r x y z) (ang rot)
;    (print (list (* 2 (asin r)) x y z))
    (gl:rotate (/ (* 360 (acos r)) pi) x y z)))

(defun qspin (q p)
  (qnormalized (q* p q)))

(defmacro incrot (place angle)
  `(setf ,place (qspin ,place ,angle)))

(defmethod spin (angled angle)
  (incrot (ang angled) angle))

(defun qtest ()
  (clg-spat::qspin (quat:quaternion-from-axis-angle #(0 0 1) pi)
                     (quat:quaternion-from-axis-angle #(0 0 1) (* pi 1))))

(defun qangle (q)
  (bind-quaternion (r i j k) q
    (declare (ignore i j k))
    (sin (/ r 2))))

;;;rotator

(defgeneric boost (rotating angle))

(defun boost2 (rota rotd angle &optional (faca 1) (facd (/ faca)))
  (boost rota (q* angle (quaternion faca 0 0 0)))
  (boost rotd (q* angle (quaternion facd 0 0 0))))

(defclass rotator ()
  ((rot :accessor rot)))

(defmethod initialize-instance :after ((ins rotator) &key rot)
  (setf (slot-value ins 'rot)
        (ensure-quaternion rot)))

(defmethod boost (rotator angle)
  (incrot (rot rotator) angle))

(defmethod act :after ((rot rotator))
  (spin rot (rot rot)))


;;;collide

(defgeneric collide (posa posd)
  (:method-combination progn))

(defgeneric single-touch-distance (posa))

(defgeneric touch-distance (posa posd)
  (:documentation "Computes the needed distance of two objects to touch each other")
  (:method (posa posd)
    (+ (single-touch-distance posa)
       (single-touch-distance posd))))

(defgeneric current-vector (posa posd)
  (:documentation "Computes the current distance of two objects as a Vector"))

(let ((*package* (find-package :cl))) 
  (define-method-combination * :identity-with-one-argument t))

(defgeneric collide-factor (self other)
  (:method-combination *)
  (:method * (self other) 1))

(defun current-distance (posa posd)
  "Computes the current distance of two objects,
depends on the generic function #'CURRENT-VECTOR"
  (absvec (current-vector posa posd)))

(defun check-collision (posa posd)
  (<= (current-distance posa posd) (touch-distance posa posd)))

(defclass collider () ())

(defmethod current-vector ((posa positional) (posd positional))
  (v- (pos posd) (pos posa)))


(defmethod interact progn ((posa collider) (posd collider))
  (if (check-collision posa posd)
      (collide posa posd)))


;;;repel

(defgeneric repel (posa posd acc faca facd))

(define-method-combination v+ :identity-with-one-argument t)

(defgeneric repel-vector (posa posd)
  (:method-combination v+)
  (:documentation
   "Computes a vector representing the speed and direction of acceleration between two objects"))

(defgeneric repel-factor (self other)
  (:method-combination *)
  (:method * (self other) (collide-factor self other)))

(defun distance-vector (target vector
                        &aux (distance (absvec vector)))
  "Computes the minimal vector to "
  (v* vector
      (if (zerop distance)
          0
          (/ (- distance target) distance))))

(defun edge-vector (posa posd)
  (distance-vector (touch-distance posa posd)
                   (current-vector posa posd)))

(defclass repeller (collider) ())

(defmethod repel-vector v+ ((posa collider) (posd collider))
  (edge-vector posa posd))

(defmethod collide progn ((posa repeller) (posd repeller))
  (repel posa posd (repel-vector posa posd) (repel-factor posa posd) (repel-factor posd posa)))

(defmethod repel (posa posd acc faca facd)
  (accelerate2 posa posd acc faca facd))

#+nil
(defmethod repel ((posa repeller) posd acc)
  (accelerate posa acc))
#+nil
(defmethod repel (posa (posd repeller) acc)
  (accelerate posd (v- acc)))

;;(defmethod repel (

;;;soft

(defclass soft (repeller) ((hardness :accessor hardness :initarg :hardness)))

(defmethod repel-vector :around ((posa soft) (posd repeller))
  (v* (call-next-method) (hardness posa)))

(defmethod repel-vector :around ((posa repeller) (posd soft))
  (v* (call-next-method) (hardness posd)))


;;;rolling

(defun vector-angle (roll side)
  (quaternion-from-axis-angle
   (unitvec (cross3 roll side))
   (line-distance roll side)))

(defun angle-vector (ang side)
  (bind-quaternion (a x y z) ang
    (if (= a 1)
        (vector-of 'real 0 0 0)
        (let* ((vec (vector-of 'real x y z))
               (dir (cross3 side vec))
               (abs (absvec dir))
               (acc (sin (angle vec side))))
          (if (or (zerop abs) (zerop acc))
              (vector-of 'real 0 0 0)
              (v* (v/ dir abs) acc 2 (acos a)))))))

(defun vav (roll side)
  (angle-vector (vector-angle roll side) side))

(defgeneric roll (rota rotd bs faca facd))

(defgeneric roll-vector (rota rotd)
  (:method-combination v+))

(defmethod roll-vector :around (rota rotd)
  (call-next-method))

(defgeneric roll-angle (rota rotd)
  (:method (rota rotd)
    (vector-angle (roll-vector rota rotd)
                  (current-vector rota rotd))))

(defgeneric roll-factor (self other)
  (:method-combination *)
  (:method * (self other) (collide-factor self other)))

(defmethod roll (rota rotd bs faca facd)
  (boost2 rota rotd bs faca facd))

(defclass roller (collider) ())

(defmethod collide progn ((rota roller) (rotd roller))
  (roll rota rotd (roll-angle rota rotd) (roll-factor rota rotd) (roll-factor rotd rota)))


;;;smooth

(defclass smooth (roller)
  ((roughness :accessor roughness :initarg :roughness)))

(defmethod roll-vector :around ((rota roller) (rotd smooth))
  (v* (call-next-method) (roughness rotd)))

(defmethod roll-vector :around ((rota smooth) (rotd roller))
  (v* (call-next-method) (roughness rota)))


;;;mass


(defclass physical () ((mass :accessor mass :initarg :mass)))

(defmethod collide-factor * ((posa physical) (posd physical))
  (mass posd))

(defmethod repel-vector :around ((posa physical) (posd physical))
  (v/ (call-next-method) (+ (mass posa) (mass posd))))

(defmethod roll-vector :around ((posa physical) (posd physical))
  (v/ (call-next-method) (+ (mass posa) (mass posd))))

#|
;;;gravity

(defvar *gravity* 0)

(defclass graviton (physical) ())

(defmethod interact progn ((posa graviton) (posd graviton))
  (accelerate2 posa posd (v* vec strength (/ (expt distance 3))) (mass posd) (mass posa)))

|#

;;;scaled

(defclass scaled (transformed)
  ((size :accessor size
         :initarg :size
         :type real)))

(defmethod draw :before ((pos scaled) &aux (size (size pos)))
  (gl:scale size size size))

(defmethod roll-factor * ((self scaled) other)
  (/ (size self)))

(defmethod single-touch-distance ((ball scaled))
  (size ball))

;;;ball

(defclass ball (scaled) ())

(defmethod draw ((ball ball))
  (gl:with-pushed-matrix
    (draw-circle)
    (gl:rotate 90 1 0 0)
    (draw-circle)
    (gl:rotate 90 0 1 0)
    (draw-circle)))

(defmethod roll-vector v+ ((posa ball) posd)
  (angle-vector (rot posa) (current-vector posa posd)))

(defmethod roll-vector v+ ((posa ball) posd)
  (angle-vector (rot posd) (current-vector posd posa)))

(defmethod roll-vector v+ ((posa mover) mover)
  (v- (vel posa)))

(defmethod roll-vector v+ (posa (posd mover))
  (vel posd))

#+nil
(defmethod roll :before ((balla ball) (balld ball) bs faca facd)
  (bind-quaternion (a i j k) bs
    (repel balla balld (v* (unitvec (cross3 (current-vector balla balld)
                                            (vector-of 'real i j k)))
                           2 (acos a))
           faca facd)))
                                            

#+nil
(defun roundd (num div)
  (* (round num div) div))

;;;

(defclass rotatable-ball (rotated rotating rollable ball) ())




;;;color

(defclass colored ()
  ((color :accessor color :initform (list 1 1 1) :initarg :color)))

(defmethod draw :around ((colored colored))
  (gl:with-pushed-attrib (:current-bit)
    (apply #'gl:color (color colored))
    (call-next-method)))
  


;;;test

(defclass test-ball (colored rotated ball) ())

(defmethod draw :after ((ball test-ball))
  (gl:color 0 0 0)
  (gl:with-primitive :lines
    (gl:vertex 0 0 0)
    (gl:vertex 1 0 0)))


#|
#|

(defclass connected (mover)
  ((line-acc :initform nil :accessor line-acc)
   (line-num :initform 0 :accessor line-num)))

(defmethod accelerate ((pos connected) vector)

(defun line-accelerate
  (if (line-acc pos)
    (incf (line-acc pos) vector)
    (setf (line-acc pos) vector)))
  
(defmethod act :after ((pos connected))
  
|#

;;;line

(defclass line ()
  ((posa :initarg :posa :type 'positional)
   (posd :initarg :posd :type 'positional)))

(defmethod collide ((linea line) (lined line))
  (with-slots (posa posd) linea
    ()))

(defmethod draw ((line line))
  (with-slots (posa posd) line
    (draw-line (pos posa) (pos posd))))

(defmethod single-touch-distance ((line line))
  0)



(flet ((vec (pos line)
         (with-slots (posa posd) line
           (let* ((vals (list (pos posd) (v- (pos posa) (pos posd)) (pos pos)))
                  (fac (apply #'orthogonal-length vals)))
             (if (<= 0 fac 1)
                 (apply #'line-vector vals)
                 (if (< fac 0)
                     (v- (pos posd) (pos pos))
                     (v- (pos posa) (pos pos))))))))
  (defmethod current-vector ((pos positional) (line line))
    (vec pos line))
  (defmethod current-vector ((line line) (pos positional))
    (v- (vec pos line))))


(defmethod accelerate ((line line) acc)
  (with-slots (posa posd) line
    (let ((acc (v/ acc 2)))
      (accelerate posa acc)
      (accelerate posd acc))))

(flet ((acc2 (pos line)
         (with-slots (posa posd) line
           (if-let ((acc (compute-acceleration line pos)))
             (let ((fac (orthogonal-length (pos posa) (v- (pos posd) (pos posa)) (pos pos))))
               (accelerate pos (v- acc))
               (if (<= 0 fac 1)
                 (progn
                   (accelerate posd (v* acc fac))
                   (accelerate posa (v* acc (- 1 fac))))
                 (if (< fac 0)
                   (accelerate posa acc)
                   (accelerate posd acc))))))))
  (defmethod collide ((pos collider) (line line))
    (acc2 pos line))
  (defmethod collide ((line line) (pos collider))
    (acc2 pos line)))

;;;connection

(defclass connection (line)
  ((length :initarg :length :type real)
   (strength :initarg :strength :initform 1 :type real)))

(defmethod act ((connection connection))
  (with-slots (posa posd length strength) connection
    (let ((acc (v* (relative-vector length (v- (pos posd) (pos posa))) strength)))
      (accelerate2 posa acc))))


;;;start

(defun create (class &rest init-args)
  (create-instance class *objects* init-args))

(defun run ()
  (act *objects*))

(defun display ()
  (draw *objects*))


;;;

;;;test

(defclass test-ball (mover ball collider colored) ())

(defmethod print-object ((ball test-ball) stream)
  (print-unreadable-object (ball stream :type ball)
    (format stream "pos: ~a; vel: ~a" (pos ball) (vel ball))))

(defmethod act ((ball test-ball)))

(defclass bridge (connection collider colored) ())

(defclass accelerator () ((acc :accessor acc :initarg :acc)))

(defmethod* interact ((acc accelerator) (pos positional))
  (accelerate pos (acc acc)))

(defclass re () ((re :accessor re :initarg :re)))

(defmethod* interact ((re re) (pos mover))
  (mulv (vel pos) (re re)))



#+nil
(defmethod draw :before ((pos test-ball))
  (dolist (o *objects*)
    (ignore-errors
      (if-let ((v (current-vector pos o)))
        (draw-line (pos pos) (v+ (pos pos) v))))))


(define-modify-macro mulf (value) *)

(defun interactive-test-init ()
  (print (list :balls :line))
  (test-init (read)))



;;(setq clg-gl::*circle-quality* 5)

(defvar *frame-rate* 32)

(defmacro with-key-char (char key &body body)
  `(let ((,char (handler-case (code-char (sdl2:sym-value ,key)) (error nil))))
     (when ,char ,@body)))

    


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


(defun test-init (input)
  (setq *keys* nil)
  (reset-hold-actions)
  (reset-press-actions)
  (reset-release-actions)
  (setq *objects* nil)
  (define-press-action #\r
    (test-init input))
  (define-press-action #\Escape
      (sdl2:push-event :quit))
  (case input
    (:balls
     (dotimes (i 16)
       (let ((j (+ (* i 32) 64)))
         (create 'test-ball
                 :color (list (random 1.0) (random 1.0) (random 1.0))
                 :pos (vector (* j j (/ 256)) j)
                 :vel (let ((r (if (oddp i) 1 -1))) (vector (/ r (1+  i)) (/ (* 2/3 r) (- 16 i))))
                 :scale 16))))
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
       (setq last (create 'positional :pos (vector -512 0)))
       (dotimes (i 15)
         (if t; (evenp i)
           (create 'test-ball :pos (vector (+ -512 (* 64 (1+ i))) -16)
                              :vel (vector 0 0)
                              :scale 16
                              :color (list (random 1.0) (random 1.0) (random 1.0))))
         (setq current (create 'mover :pos (vector (+ -512 (* 64 (1+ i))) 0)
                                      :vel (vector 0 0)))
         (create 'bridge :posa last :posd current :length 32 :strength 1/4)
         (setq last current))
       (setq current (create 'positional :pos (vector 512 0)))
       (create 'bridge :posa last :posd current :length 32 :strength 1/4)
       (create 'accelerator :acc #(0 1/2))
       (create 're :re 63/64)))
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
    (:snake
     (let (last current)
       (setq last (create 'test-ball :pos (vector -512 0)))
       (dotimes (i 15)
         (setq current (create 'test-ball :pos (vector (+ -512 (* 64 (1+ i))) 0))))))))


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






;;;draw





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



 (defmacro define-printer (name class text &rest options
                                   &aux (object (gensym "OBJECT")))
           `(defmethod ,name ,@options ((,object ,class))
                       (princ ,text)))
                             
|#
