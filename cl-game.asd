(asdf:defsystem #:cl-game
  :description ""
  :license "LLGPL"
  :depends-on (#:alexandria #:cl-opengl #:cl-glut #:mixalot #:trivial-garbage #:named-readtables #:png
                            #:inferior-shell #:usocket #:bordeaux-threads)
  :components ((:file "util")
               (:file "encryption")
               (:file "web/package" :depends-on ("encryption" "util"))
               (:file "web/server" :depends-on ("web/package"))
               (:file "web/client" :depends-on ("web/package"))
               (:file "vectors" :depends-on ("util"))
               (:file "rotate" :depends-on ("vectors"))
               (:file "opengl" :depends-on ("rotate"))
               (:file "define-instance")
               (:file "events" :depends-on ( "util" "define-instance"))
               (:file "talk" :depends-on ("util"))
               (:file "sound")
               (:file "spatial" :depends-on ("util" "opengl" "vectors" "rotate" "sound"))
               (:file "actors" :depends-on ("util" "spatial"))
               (:file "input" :depends-on ("util"))
               (:file "window" :depends-on ("spatial" "input"))
               (:file "clos") 
               (:file "game" :depends-on ("rotate" "spatial" "actors" "window" "clos"))))



