(asdf:defsystem #:cl-game
  :description ""
  :license "LLGPL"
  :depends-on (#:alexandria #:cl-opengl #:cl-glut #:bordeaux-threads #:cl-quaternion)
  :components ((:file "util")
               (:file "vectors" :depends-on ("util"))
               (:file "opengl")
               (:file "spatial" :depends-on ("util" "vectors" "opengl"))
               (:file "input" :depends-on ("util"))
               (:file "window" :depends-on ("spatial" "input"))
               (:file "game" :depends-on ("spatial" "window"))))



