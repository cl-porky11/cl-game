# cl-game
a common lisp very easy estensible game engine

two test-games avialble; start them with these expressions:
  (clg:start 'clg::init-window 'clg::init-) ;;first game
  (clg:start 'clg::init-window 'clg::init-cam 'clg::init-flyballs) ;;second game

fist: control with arrow-keys or WASD
second: rotate with arrow-keys, move with WASD, move forward with space

the second one doesn't work good, gl:ortho and gl:frustrum don't do what I expect


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
  etc. (see the exported classes from "spatial.lisp")

load with quicklisp after downloading/cloning:
  (ql:quickload :cl-game)

defining classes:
  (defclass test-ball (positional mover ball colored) ())
  ;;a colored ball with a position that moves

  (defclass test-error (mover ball colored) ())
  ;;this will cause an error because mover will call the method move, which is implemented positional
  ;;mover doesnt inherit from positional that you can define other classes like positional, that implement this method

making instances
  (make-instance 'test-ball :pos (vector 0 0 0) :vel (vector 0 0 0) :size 16 :color '(1 0 0))
  ;;a red colored ball at position (0|0|0) with speed (0|0|0) and radius 16
  ;;vectors may be also of other dimensions. rotation may only work 3dimensional

when writing something like following, classes are defined automatically:
  (make-instance '(positional mover ball colored) :pos (vector 0 0 12))
  ;;see amop, make-programmatic-class or the file "clos.lisp"

