(asdf:defsystem #:cl-game
  :description ""
  :license "LLGPL"
  :depends-on (#:alexandria #:cl-opengl #:cl-glut #:bordeaux-threads #:cl-quaternion)
  :components ((:file "util")
               (:file "vectors" :depends-on ("util"))
               (:file "rotate" :depends-on ("vectors"))
               (:file "opengl" :depends-on ("rotate"))
               (:file "spatial" :depends-on ("util" "opengl" "vectors" "rotate"))
               (:file "input" :depends-on ("util"))
               (:file "window" :depends-on ("spatial" "input"))
               (:file "clos") 
               (:file "game" :depends-on ("rotate" "spatial" "window" "clos"))))



