(in-package :sdl2-engine)

(defun sprite-pixels-from (surface index width height
                           &key (transparent-color '(0 0 0)))
  "Return an array of integers : pixels of the sprite at index in the sheet"
  (with-rect (rect-dst 0 0 width height)
    (multiple-value-bind (y x) (floor index (floor (sdl2:surface-width surface) width))
      (with-rect (rect-src (* x width) (* y height) width height)
        (let* (
	       (depth 32)
	       (surface-dest (sdl2:create-rgb-surface width height depth))
	       (sdl-pixels-pointer (sdl2:surface-pixels surface-dest)))
          (sdl2:set-color-key surface +sdl-true+
	        	      (sdl2:map-rgb (sdl2:surface-format surface)
                                            (first transparent-color)
                                            (second transparent-color)
                                            (third transparent-color)))
          (sdl2:blit-surface surface
		             rect-src
		             surface-dest
		             rect-dst)
          (cffi:foreign-array-to-lisp
	   sdl-pixels-pointer
	   `(:array :int32 ,(* width height))))))))


