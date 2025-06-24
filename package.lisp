(defpackage #:sdl2-engine
  (:use #:cl #:alexandria)

  (:export 
	   ;; Informations
           #:display-version
           ;; Engine
           #:engine
           #:add-asset
           #:reset-asset
           #:handler-loop
           #:handler-quit
           #:default-draw-color
           #:launch
           #:with-ticks
           ;; Sprites Sheet
           #:draw-sprite
           #:draw-line
           #:draw-rect
           ;; Stencils Sheet
           #:create-stencils
           ;; text
           #:with-text-asset
           #:add-text
           #:add-text-digits
           #:draw-text
           #:draw-texts
           #:draw-number
           ;; Keyboard
           #:add-keyboard-mapping
           #:del-keyboard-mapping
           #:reset-keyboard-mapping
           #:enable-keyboard-key
           #:disable-keyboard-key
           #:enable-keyboard-controls
           #:disable-keyboard-controls
           ;; Physics
           #:intersectp
           #:stencils-intersectp
           ))
