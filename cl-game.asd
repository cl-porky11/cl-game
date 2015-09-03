(asdf:defsystem #:cl-game
  :description ""
  :license "LLGPL"
  :depends-on (#:alexandria #:cl-opengl #:cl-glut #:bordeaux-threads #:cl-quaternion)
  :components ((:file "util")
               (:file "vectors")
               (:file "opengl")
               (:file "spatial" :depends-on ("util" "vectors" "opengl"))
               (:file "window" :depends-on ("spatial"))
               (:file "game")))



