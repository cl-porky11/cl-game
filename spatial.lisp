(defpackage #:clg-spat
  (:use #:cl #:clg-util #:clg-vec #:clg-rot #:clg-sound #:clg-gl #:alexandria)
  (:import-from #:glut
                #:display)
  (:export #:display
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

           #:turn
           #:angled
           #:rot

           #:boost
           #:boost2
           #:rotator
           #:spin

           #:slow
           #:breaker
           #:friction

           #:collide
           #:single-touch-distance
           #:touch-distance
           #:current-vector
           #:interact-factor
           #:current-distance
           #:check-collision
           #:collider

           #:repel
           #:repel-vector
           #:repel-factor
           #:distance-vector
           #:repeller

           #:soft
           #:hardness

           #:roll
           #:roll-rotation
           #:roll-vector
           #:roll-factor
           #:roller

           #:smooth
           #:roughness

           #:physical
           #:mass
           #:*gravity*

           #:scaled
           #:size

           #:ball
           #:colored

           #:camera
           #:focusing
           #:*front-vector*
           #:looking
           
           #:cluster
           #:interact-cluster
           #:gravity-cluster

           #:line
           #:connection

           #:last-collision
           #:last-collider
           ))
(in-package #:clg-spat)

;;;etc.

(defmacro show (&rest vars)
  `(progn (format t ,(format nil "~{~a: ~~a~^; ~}~%" vars) ,@vars)
          (list ,@vars)))

(define-method-combination single ()
  ((primary (:single) :required t))
  (if (cdr primary)
      (method-combination-error
       "More than one method was implemented for a method-combination of type single")
      `(call-method ,(car primary))))

(defun single-symbol-qualifier-p (list)
  (destructuring-bind (symbol . rest) list
    (and (symbolp symbol)
         (not (eq (symbol-package symbol) (find-package symbol)))
         (null rest))))

(defun number-qualifier-p (list)
  (destructuring-bind (car . cdr) list
    (and (realp car)
         (null cdr))))

(define-method-combination numbered (operation &key (order :most-specific-first) (predicate #'<))
  ((around (:around))
   (primary ())
   (methods number-qualifier-p :order order))
  `(,operation
    ,@(mapfun ((method primary)) `(call-method ,method))
    ,@(mapcar (lambda (num)
                (destructuring-bind
                      (first . rest)
                    (remove-if-not
                     (lambda (method) (= num (car (method-qualifiers method))))
                     methods)
                  `(call-method ,first ,rest)))
              (stable-sort (let (nums)
                             (map nil (lambda (method)
                                        (pushnew
                                         (car (method-qualifiers method))
                                         nums))
                                  methods)
                             nums)
                           predicate))))
#+nil
(define-method-combination numbered (operation &key (order :most-specific-first) sort-by)
  ((around (:around))
   (methods single-symbol-qualifier-p :order order))
  `(,operation
     ,@(maphash
        (lambda (key value)
          (declare (ignore key))
          (
        (table-group methods :test 'eq :key (lambda (method)
                                              (car (method-qualifiers method)))))))))
  

#+nil
(define-method-combination progn+ (&key required (order :most-specific-first) (allow-around t))
  ((primary (progn) :order order)
   (around (:around) :order order))
  (if (and required (null primary))
      (method-combination-error "no primary method defined, but required"))
  (if (and around (not allow-around))
      (method-combination-error "around-methods are not allowed here"))
  `(call-method (make-method ,@around (call-method ,@primary))))

;;;default-methods

(defmethod display (object))

(defgeneric act (object)
  (:method-combination progn)
  (:method progn (object) :most-specific-last))

(defgeneric interact (objecta objectd)
  (:method-combination progn)
  (:method progn (a d)))


(defmethod act progn ((object list))
  (loop for (object . rest) on object
     do (act object)
     do (interact object rest)))

(defmethod display ((objects list))
  (dolist (object (reverse objects))
    (display object)))

(defmethod interact :around ((actora list) actord)
  (dolist (actor actora)
    (interact actor actord)))

(defmethod interact :around (actora (actord list))
  (dolist (actor actord)
    (interact actora actor)))


;;;classes

(defclass transformed () ())

(defmethod display :around ((some transformed))
  (gl:with-pushed-matrix
    (call-next-method)))

(defvar *default-dimension* 3)

(defclass dimensional ()
  ((dim :initarg :dim :initform *default-dimension*)))


;;;positions

(defgeneric move (pos vector)
  (:method-combination single))

(defclass positional (transformed)
  ((pos :accessor pos
        :initarg :pos
        :initform (vector 0 0 0)
        :type (vector real))))

(defaccessor pos- (positional number)
  (aref (pos positional) number))

(defmethod display :before ((pos positional))
  (gl:translate (pos- pos 0) (pos- pos 1) (pos- pos 2)))

(defmethod move :single ((pos positional) vector)
  (incv (pos pos) vector))


;;;movement

(defgeneric accelerate (pos vector)
  (:method-combination single))

(defun accelerate2 (movera moverd acc &optional (faca 1) (facd (/ faca)))
  (accelerate movera (v* acc faca))
  (accelerate moverd (v- (v* acc facd))))

(defclass mover ()
  ((vel :accessor vel
        :initarg :vel
        :initform (vector 0 0 0)
        :type (vector real))))

#+nil
(defmethod initialize-instance ((pos mover) &key)
  (with-slots (dim) pos
    (if (slot-unbound 'positional pos 'vel)
        (setf (slot-value pos 'vel) (make-array dim :initial-element 0)))))

(defaccessor vel- (positional number)
  (aref (vel positional) number))

(defmethod act progn ((mover mover))
  (move mover (vel mover)))

(defmethod accelerate :single ((mover mover) vector)
  (incv (vel mover) vector))


;;;angeled

(defgeneric turn (object vector)
  (:method-combination single))

(defclass angled (transformed)
  ((rot :accessor rot :initarg :rot :type rotation
        :initform (make-rotation :angle 0 :axis #(0 0 1)))))

(defmethod display :before ((rot angled))
  (with-accessors ((r rot)) rot
    (rotate-with-rotation r)))


(defmethod turn :single (angled rot)
  (mulr (rot angled) rot)
  )

;;;rotator

(defgeneric boost (rotating rot)
  (:method-combination single))

(defun boost2 (rota rotd rot &optional (faca 1) (facd (/ faca)))
  (boost rota (v* rot faca))
  (boost rotd (v* rot facd))
  )

(defclass rotator ()
  ((spin :accessor spin :initarg :spin
         :initform (make-rotation :angle 0 :axis #(0 0 1)))))

(defmethod boost :single (rotator rot)
  (incv (spin rotator) rot)
  )

(defmethod act progn ((rot rotator))
  (turn rot (spin rot)))

;;;friction

(defgeneric slow (obj)
  (:method-combination progn))

(defclass breaker ()
  ((friction :accessor friction :initarg :friction :initform 1)))

(defmethod act progn ((rot breaker))
  (slow rot))

(defmethod slow progn ((mover mover))
  (mulv (vel mover) (friction mover)))

(defmethod slow progn ((rot rotator))
  (mulv (spin rot) (friction rot)))

;;;collide

(defgeneric collide (posa posd)
  (:method-combination numbered progn))

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

(defgeneric interact-factor (self other)
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

(defgeneric single-repel (posa posd acc faca facd))

(define-method-combination v+ :identity-with-one-argument t)

(defgeneric repel-vector (posa posd)
  (:method-combination v+)
  (:documentation
   "Computes a vector representing the speed and direction of acceleration between two objects"))

(defgeneric repel-factor (self other)
  (:method-combination *)
  (:method * (self other) (interact-factor self other)))

(defun distance-factor (distance target)
  (if (zerop distance)
      0
      (/ (- distance target) distance)))

(defun factor-vector (vector fun &rest args)
  (v* vector (apply fun (absvec vector) args)))

(defun distance-vector (target vector)
  "Computes the minimal vector to "
  (factor-vector vector 'distance-factor target))

(defclass repeller (collider) ())

(defmethod repel-vector v+ ((posa collider) (posd collider))
  (distance-vector (touch-distance posa posd)
                   (current-vector posa posd)))

(flet ((rep (posa posd)
         (repel posa posd (repel-vector posa posd) (repel-factor posa posd) (repel-factor posd posa))))
  (defmethod collide 1 ((posa collider) (posd repeller))
             (rep posa posd))
  (defmethod collide 1 ((posa repeller) (posd collider))
             (rep posa posd)))

(defmethod single-repel (posa posd acc faca facd))

(defmethod single-repel ((posa repeller) posd acc faca facd)
  (accelerate posa (v* acc (+ faca facd))))

(defmethod single-repel ((posa repeller) (posd repeller) acc faca facd)
  (accelerate posa (v* acc faca)))

(defun repel (posa posd acc faca facd)
  (single-repel posa posd acc faca facd)
  (single-repel posd posa (v- acc) facd faca))


;;;soft

(defclass soft (repeller) ((hardness :accessor hardness :initarg :hardness :initform 1)))

(defmethod repel-vector :around ((posa soft) (posd repeller))
  (v* (call-next-method) (hardness posa)))

(defmethod repel-vector :around ((posa repeller) (posd soft))
  (v* (call-next-method) (hardness posd)))


;;;rolling

(defgeneric roll (rota rotd bs faca facd))

(defgeneric single-roll-vector (self other vector dis)
  (:method-combination v+)
  (:method v+ (self other vector dis) (vector 0 0 0)))

(defun roll-vector (rota rotd vector disa disd)
  (v- (single-roll-vector rota rotd vector disa)
      (single-roll-vector rotd rota (v- vector) disd)))

(defgeneric roll-rotation (rota rotd vector disa disd)
  (:method (rota rotd vector disa disd)
    (vector-rotation (roll-vector rota rotd vector disa disd)
                     vector)))

(defgeneric roll-factor (self other)
  (:method-combination *)
  (:method * (self other) (interact-factor self other)))

(defclass roller (collider) ())

(defmethod single-roll (rota rotd bs faca facd))

(defmethod single-roll ((rota roller) rotd bs faca facd)
  (boost rota (v* bs (+ faca facd))))

(defmethod single-roll ((rota roller) (rotd roller) bs faca facd)
  (boost rota (v* bs faca)))

(defmethod roll (rota rotd bs faca facd)
  (single-roll rota rotd bs faca facd)
  (single-roll rotd rota bs facd faca))

(flet ((ro (rota rotd)
         (roll rota rotd (roll-rotation rota rotd (current-vector rota rotd)
                                        (single-touch-distance rota)
                                        (single-touch-distance rotd))
               (roll-factor rota rotd) (roll-factor rotd rota))))
  (defmethod collide 2 ((rota roller) (rotd collider))
             (ro rota rotd))
  (defmethod collide 2 ((rota collider) (rotd roller))
             (ro rota rotd)))


;;;smooth

(defclass smooth (roller)
  ((roughness :accessor roughness :initarg :roughness :initform 1)))

(defmethod roll-factor * ((self smooth) other)
  (roughness self))

(defmethod roll-factor * (self (other smooth))
  (roughness other))


;;;mass


(defclass physical () ((mass :accessor mass :initarg :mass)))

(defmethod interact-factor * ((posa physical) (posd physical))
  (/ (mass posd) (+ (mass posa) (mass posd))))


;;gravity

(defgeneric single-attract (posa posd acc faca facd))

(defgeneric attract-factor (self other)
  (:method-combination *)
  (:method * (self other)
    (interact-factor self other)))

(defgeneric attract-vector (posa posd vector))

(defvar *gravity* 0)

(defclass faller () ())

(defun attract (posa posd acc faca facd)
  #+nil
  (if (< 16 (mass posd))
      (show acc))
  (single-attract posa posd acc faca facd)
  (single-attract posd posa (v- acc) facd faca))

(defmethod interact progn ((posa physical) (posd physical))
  (unless (zerop *gravity*)
    (attract posa posd (attract-vector posa posd (current-vector posa posd))
             (attract-factor posa posd) (attract-factor posd posa))))

(defun gravity-factor (distance factor power)
  (* factor (expt distance power)))

(defmethod attract-factor * (self (other physical))
  (mass other))

(defmethod attract-vector (posa posd vector)
  (factor-vector vector 'gravity-factor *gravity* -3))

(defmethod single-attract ((posa physical) (posd physical) acc faca facd)
  #+nil
  (if (< 16 (mass posd))
      (show acc faca facd))
  (accelerate posa (v* acc faca)))

(defmethod single-attract ((posa physical) posd acc faca facd)
  (accelerate posa (v* acc (+ faca facd))))

(defmethod single-attract (posa posd acc faca facd))


;;;scaled

(defclass scaled (transformed)
  ((size :accessor size
         :initarg :size
         :initform 1
         :type real)))

(defmethod display :before ((pos scaled) &aux (size (size pos)))
  (gl:scale size size size))

(defmethod roll-factor * ((self scaled) other)
  (/ (single-touch-distance self)))

(defmethod single-touch-distance ((ball scaled))
  (size ball))


;;;ball

(defclass ball () ())

(defmethod display ((ball ball))
  ;;  (let ((clg-gl::*circle-quality* (round (* 2 (sqrt (size ball))))))
    (gl:with-pushed-matrix
      (draw-circle)
      (gl:rotate 90 1 0 0)
      (draw-circle)
      (gl:rotate 90 0 1 0)
      (draw-circle)));)

(defmethod single-roll-vector v+ ((self rotator) other vector dis)
  (v* (rotation-vector (spin self) vector) dis))

(defmethod single-roll-vector v+ ((self mover) other vector dis)
  (v- (vel self)))

#+nil
(defmethod roll :before (posa posd bs faca facd)
  #+nil
  (if (< 16 (mass posd))
      (show bs))
  (repel posa posd
         (rotation-vector bs (current-vector posd posa))
         faca facd))

(defmethod single-roll :before (posa posd bs faca facd)
  (accelerate posa
              (v* (rotation-vector bs (current-vector posd posa)) faca)))


;;;color

(defclass colored ()
  ((color :accessor color :initform (list 1 1 1) :initarg :color)))

(defmethod display :around ((colored colored))
  (gl:with-pushed-attrib (:current-bit)
    (apply #'gl:color (color colored))
    (call-next-method)))


;;camera

(defclass camera ()
  ((draw-object :initarg :draw-object
                :initarg :object)))

(defmethod display ((cam camera))
  (with-slots (draw-object) cam
    (display draw-object)))

(defclass focusing (camera transformed)
  ((focus-object :initarg :focus-object)))

(defmethod display :before ((cam focusing))
  (with-slots ((pos focus-object)) cam
    (gl:translate (- (pos- pos 0))
                  (- (pos- pos 1))
                  (- (pos- pos 2)))))

(defvar *front-vector* (vector 0 0 1))

(defclass looking (camera transformed) 
  ((focus-object :initarg :focus-object)
   (look-object :initarg :look-object)))

(defmethod display :before ((cam looking))
  (with-slots ((posa focus-object) (posd look-object)) cam
    (rotate-with-rotation
     (v- (rotation-to-vector *front-vector* (current-vector posa posd))))))


;;mesh

#+nil
(defclass mesh ()
  (vertices
   indices))

#+nil
(defmethod initialize-instance :after ((mesh mesh) &key new-vertices new-indices)
  (with-slots (vertices indices) mesh
    (setf vertices (gl:alloc-gl-array :float (length new-vertices)))
    (setf indices (gl:alloc-gl-array :unsigned-short (length new-indices)))))

#+nil
(defmethod display ((mesh mesh))
  (with-slots (vertices indices) mesh))
    
    
;;(make-array 9 :displaced-to #2a((1 2 3) (4 5 6) (7 8 9)))


#|
    
(defclass looking4 (camera angled)
  ((focus-object :initarg :focus-object)
   (look-object :initarg :look-object)))

(defmethod act :around ((cam looking4))
  (with-slots ((posa focus-object) (posd look-object) rot) cam
    (let ((new-front (current-vector posa posd)))
      (setf rot (rotation-to-vector *front-vector* new-front))
      (let ((*front-vector* new-front))
        (call-next-method)))))

(defmethod display :before ((cam looking4))
  (with-slots ((posa focus-object) (posd look-object) rot) cam
    (mulv rot -1)))

(defmethod display :after ((cam looking4))
  (with-slots ((posa focus-object) (posd look-object) rot) cam
    (mulv rot -1)))

(defclass looking2 (camera angled)
  ((focus-object :initarg :focus-object)
   (look-object :initarg :look-object)))


(defmethod display :before ((cam looking2))
  (with-slots ((posa focus-object) (posd look-object) rot) cam
    (setf rot (v* (v- (rotation-to-vector (rotate-vector #(0 0 1) rot) (current-vector posa posd))) 2))))

(defmethod display :after ((cam looking2))
  (with-slots ((posa focus-object) (posd look-object) rot) cam
    (setf rot (v- (v/ rot 2)))))

(defclass looking3 (camera angled rotator)
  ((focus-object :initarg :focus-object)
   (look-object :initarg :look-object)))

(defmethod act progn ((cam looking3))
  (with-slots ((posa focus-object) (posd look-object) rot) cam
    (incv rot (v/ (rotation-to-vector (rotate-vector #(0 0 1) rot) (current-vector posa posd)) 8))))

(defmethod display :before ((cam looking3))
  (with-slots ((posa focus-object) (posd look-object) rot) cam
    (setf rot (v- rot))))

(defmethod display :after ((cam looking3))
  (with-slots ((posa focus-object) (posd look-object) rot) cam
    (setf rot (v- rot))))

|#


;;cluster

(defclass cluster ()
  ((act-object :initarg :act-object
               :initarg :object)))

(defmethod act progn ((cl cluster))
  (with-slots (act-object) cl
    (dolist (ins (ensure-list act-object))
      (act ins))))

(defclass interact-cluster (cluster) ())

(defmethod act progn ((cl interact-cluster))
  (with-slots (act-object) cl
    (if (consp act-object)
        (loop for (object . rest) on act-object
           do (interact object rest)))))

(defclass gravity-cluster (cluster)
  ((gravity :initarg :gravity)))

(defmethod act :around ((cl gravity-cluster))
  (with-slots (gravity) cl
    (let ((*gravity* gravity))
      (call-next-method))))


;;;line

(defclass line ()
  ((pos0 :initarg :pos0)
   (pos1 :initarg :pos1)))

(defmethod display ((line line))
  (with-slots (pos0 pos1) line
    (draw-line (pos pos0) (pos pos1))))

(defmethod single-touch-distance ((line line))
  0)


(flet ((vec (pos line)
         (with-slots (pos0 pos1) line
           (let* ((vec (current-vector pos0 pos1))
                  (fac (orthogonal-factor (pos pos) vec)))
             (if (<= 0 fac 1)
                 (line-vector (pos pos) vec)
                 (if (< fac 0)
                     (current-vector pos1 pos)
                     (current-vector pos0 pos)))))))
  (defmethod current-vector ((pos positional) (line line))
    (vec pos line))
  (defmethod current-vector ((line line) (pos positional))
    (v- (vec pos line))))

(defmethod accelerate :single ((line line) acc)
  (with-slots (pos0 pos1) line
    (accelerate pos0 acc)
    (accelerate pos1 acc)))

;(defmethod repel-factor ((line line)

#+nil
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
  (defmethod collide 3 ((pos collider) (line line))
    (acc2 pos line))
  (defmethod collide 3 ((line line) (pos collider))
    (acc2 pos line)))

;;;connection

(defclass connection (line)
  ((length :initarg :length :type real)
   (strength :initarg :strength :initform 1 :type real)))

(defmethod act progn ((connection connection))
  (with-slots (pos0 pos1 length strength) connection
    (repel pos0 pos1 (v* (distance-vector length (current-vector pos0 pos1)) strength)
           (repel-factor pos0 pos1)
           (repel-factor pos1 pos0))))

;;;

(defclass hard (line)
  ((strength :initarg :strength :initform 1 :type real)))

#+nil
(defmethod act progn ((line hard))
  (with-slots (pos0 pos1 strength) line
    (let* ((vector (current-vector pos0 pos1))
           (dis (absvec vector))
           (vec0 (rotation-vector (spin pos0) vector))
           (vec1 (rotation-vector (spin pos1) vector))
           (fac0 (/ (absvec vec0) dis))
           (fac1 (/ (absvec vec1) dis))
           (roll-vec (+ (v* vec0 dis) (v* vec1 dis)
      (roll pos0 pos1 (v* (vector-rotation () vector)
                          strength)
            1 1)))))))
            

;;;last-collision

(defclass last-collision (collider)
  ((last-collider :initform nil)))

(flet ((set-last-collider (col other)
         (with-slots (last-collider) col
           (setf last-collider other))))
  (defmethod collide  ((col last-collision) (other collider))
             (set-last-collider col other))
  (defmethod collide 0 ((other collider) (col last-collision))
             (set-last-collider col other)))



(defclass sounding ()
  ((frequency :initarg :frequency :initform nil)))

(defmethod initialize-instance :after ((sounding sounding) &key)
  (with-slots (frequency) sounding
    (if frequency
        (let ((streamer (make-function-streamer (sinus-sound-function frequency 1/4))))
          (start-sound streamer)
          (tg:finalize sounding
                       (lambda () (stop-sound streamer)))))))

#|

;;;

;;;test

(defclass accelerator () ((acc :accessor acc :initarg :acc)))

(defmethod* interact ((acc accelerator) (pos positional))
  (accelerate pos (acc acc)))

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



;(loop repeat 256 do (progn (print :running) (sleep 1/32) (run)))

;(start)

;;;rest

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


;;;generics*

|#

