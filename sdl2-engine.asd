(asdf:defsystem #:sdl2-engine
  :author "Frédéric Ferrère"
  :description "SDL2 Game Engine"
  :version "1.0.0"
  :license "MIT"
  :serial t
  :depends-on (#:sdl2 #:sdl2-image #:sdl2-ttf #:array-operations #:alexandria #:bit-smasher)
  :components (
               (:file "package")
	       (:file "conf")
	       (:file "utils")
	       (:file "engine")            
               (:file "physics")
               (:file "asset")
	       (:file "sprites-sheet")
	       (:file "stencils")
	       (:file "text")
               (:file "render")
               (:file "keyboard")
               )
  :in-order-to ((asdf:test-op (asdf:test-op "sdl2-engine/test"))))

(defsystem :sdl2-engine/test
  :author "Frédéric Ferrère"
  :description "SDL2 Engine test suite"
  :version "1.0.0"
  :license "MIT"
  :depends-on (:sdl2-engine :parachute)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test :sdl2-engine/test)))


(defsystem :sdl2-engine/demo
  :author "Frédéric Ferrère"
  :description "SDL2 Engine Demo"
  :version "1.0.0"
  :license "MIT"
  :depends-on (:sdl2-engine)
  :components ((:module "demo"
                :serial t
                :components ((:file "package")
                             (:file "demo")))))
