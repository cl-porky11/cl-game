# cl-game

common lisp very easy estensible game engine

current inbuilt classes you should inherit from
  positional
  mover
  angled
  rotater
  repeller
  soft
  rotator
  smooth
  physical
  scaled
  ball
  colored

defining classes:
  (defclass test-ball (positional mover ball colored) ())
  ;;a colored ball with a position that moves

  (defclass test-error (mover ball colored) ())
  ;;this will cause an error because mover will call the method move, which is implemented positional
  ;;mover doesnt inherit from positional that you can define other classes like positional, that implement this method
  
initializing classes:
  <code>(make-instance 'test-ball :pos (vector 0 0 0) :vel (vector 0 0 0) :size 16 :color '(1 0 0))
  ;;a red colored ball at position (0|0|0) with speed (0|0|0) and radius 16
  ;;vectors may be also of other dimensions. rotation may only work 3dimensional

starting game:
    (start :objects *list-of-test-balls*)

