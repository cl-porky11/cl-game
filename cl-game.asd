(asdf:defsystem #:cl-game
  :description ""
  :license "LLGPL"
  :depends-on (#:alexandria #:cl-opengl #:cl-glut #:mixalot #:trivial-garbage #:bordeaux-threads)
  :components ((:file "util")
               (:file "vectors" :depends-on ("util"))
               (:file "rotate" :depends-on ("vectors"))
               (:file "opengl" :depends-on ("rotate"))
               (:file "sound")
               (:file "spatial" :depends-on ("util" "opengl" "vectors" "rotate" "sound"))
               (:file "actors" :depends-on ("util" "spatial"))
               (:file "input" :depends-on ("util"))
               (:file "window" :depends-on ("spatial" "input"))
               (:file "clos") 
               (:file "game" :depends-on ("rotate" "spatial" "actors" "window" "clos"))))



