(in-package :sdl2-engine)

(defconstant +sdl-true+ 1)
(defconstant +sdl-false+ 0)
(defparameter *frame-rate* 4)

(defconstant +sdl-textureaccess-static+ 0) ;;     Changes rarely, not lockable 
(defconstant +sdl-textureaccess-streaming+ 1) ;;  Changes frequently, lockable
(defconstant +sdl-textureaccess-target+ 2) ;;     Texture can be used as a render target
