(in-package :sdl2-engine)

(defmethod draw-line ((engine engine) x1 y1 x2 y2 &key (asset-name nil) color)
  (let ((renderer (engine-renderer engine)))
    (when color
      (sdl2:set-render-draw-color renderer
                                  (first color)
                                  (second color)
                                  (third color) 0))
    (when asset-name
      (let* ((asset (gethash asset-name (engine-assets engine) nil))
             (texture (sheet-texture asset)))
        (assert (typep asset 'asset-sprite))
        (sdl2:set-render-target renderer texture)))
    
    (sdl2:render-draw-line renderer x1 y1 x2 y2)
    (when asset-name
      (sdl2:set-render-target renderer nil))
    (apply #'sdl2:set-render-draw-color renderer (default-draw-color engine))))

(defmethod draw-sprite ((engine engine) sheet-name index x y &key (scale-w 1) (scale-h 1))
  (let* ((sheet (find-asset engine sheet-name))
         (width (sheet-sprite-width sheet))
         (height (sheet-sprite-height sheet)))
    (multiple-value-bind (xs ys) (sprite-coords sheet index)
      (with-rect (rect-src xs ys width height)
        (with-rect (rect-dst x y
                             (floor (* width scale-w))
                             (floor (* height scale-h)))
          (sdl2:render-copy (engine-renderer engine) (sheet-texture sheet)
                            :source-rect rect-src :dest-rect rect-dst))))))

(defmethod draw-rect ((engine engine) x y w h &key color)
  (when color
    (sdl2:set-render-draw-color (engine-renderer engine)
                                (first color)
                                (second color)
                                (third color) 0))
  (with-rect (rect x y w h)
    (sdl2:render-draw-rect (engine-renderer engine) rect))
  (apply #'sdl2:set-render-draw-color (engine-renderer engine)
         (default-draw-color engine)))

(defmethod draw-atlas-texture ((engine engine))
  "Check and debug function"
  (let* ((atlas (engine-texts engine))
         (texture (at-texture atlas))
         (renderer (engine-renderer engine)))
    (with-rect (rect 0 0 (sdl2:texture-width texture) (sdl2:texture-height texture))
      (sdl2:render-copy renderer texture :dest-rect rect))
    (loop for key being the hash-keys of (at-texts atlas) using (hash-value text)
          for x = (text-x text)
          for y = (text-y text)
          for w = (text-w text)
          for h = (text-h text)
          do (draw-rect engine x y w h :color '(255 0 0)))))


(defmethod draw-text ((engine engine) asset-name text-id x y &key (position :left))
  (let ((text (find-text engine asset-name text-id))
        (renderer (engine-renderer engine))
        (texture (at-texture (find-asset engine asset-name))))
    (when text (draw-text-at renderer texture text x y :position position))))

(defmethod draw-texts ((engine engine) asset-name texts-id x y &key (position :left))
  (loop for text-id in texts-id
        for text = (find-text engine asset-name text-id)
        with x1 = x
        do (when text
             (draw-text engine asset-name text-id x1 y :position position)
             (setf x1 (+ x1 (text-w text))))))

(defmethod draw-number ((engine engine) asset-name number x y)
  (let ((renderer (engine-renderer engine))
        (asset (find-asset engine asset-name)))
    (draw-number-at renderer asset number x y)))

