(in-package :sdl2-engine)

(defun create-text-texture (renderer texts surfaces)
  "Create texture and setup coords for every Atlas text objects"
  (let ((target-surface (target-surface surfaces))
        texture)
    (loop for k being the hash-keys of texts using (hash-value text)
          for surface in surfaces
          with x = 0
          with y = 0
          for w = (sdl2:surface-width surface)
          for h = (sdl2:surface-height surface)
          do (setup-text-coords text x y w h)
          do (with-rect (src-rect 0 0 w h)
               (with-rect (dst-rect x y w h)
                 (sdl2:blit-surface surface src-rect target-surface dst-rect)))
          do (setf y (+ y h)))
    
    (setf texture (sdl2:create-texture-from-surface renderer target-surface))
    (sdl2:free-surface target-surface)
    texture))


(defclass asset () ())

(defclass asset-text (asset)
  ((texts :accessor at-texts :initform (make-hash-table :test 'equal))
   (font :accessor at-font :initarg :font)
   (texture :accessor at-texture :initarg :texture)))

(defmethod initialize-asset ((asset asset-text) renderer)
  (let ((texts (at-texts asset))
        (font (at-font asset)))
    (initialize-font font)
    (setf (at-texture asset)
          (create-text-texture
           renderer
           texts
           (loop for k being the hash-keys of texts using (hash-value text)
                 collect (text-surface text font))))))

(defmethod add-text ((asset asset-text) id string red green blue alpha)
  (let ((text (make-instance 'text :string string
                                   :red red :green green :blue blue :alpha alpha)))
    (setf (gethash id (at-texts asset)) text)))

(defmethod add-text-digits ((asset asset-text) red green blue alpha)
  (loop for i from 0 to 9
        for string = (prin1-to-string i)
        do (add-text asset string string red green blue alpha)))

(defmethod add-text-alphabet ((asset asset-text) red green blue alpha)
  (dolist (string '("up" "down" "left" "right" "space"))
      (add-text asset string string red green blue alpha))
  (loop with first-char-code = (char-code #\a)
        for i from 0 below 26
        for string = (format nil "~a" (code-char (+ i first-char-code)))
        do (add-text asset string string red green blue alpha)))

(defmethod clear-asset ((asset asset-text))
  (sdl2:destroy-texture (at-texture asset)))

(defclass asset-stencil (asset)
  ((filename :accessor sheet-filename
             :initarg :filename)
   (data :accessor sheet-data
         :initarg :texture)
   (stencil-width :accessor stencil-width
                  :initarg :stencil-width)
   (stencil-height :accessor stencil-height
                   :initarg :stencil-height)))


(defmethod initialize-instance :after ((ss asset-stencil) &key)
  (when (slot-boundp ss 'filename)
   (setf (sheet-data ss) (load-stencils (sheet-filename ss)))))

(defmethod stencils-bits ((sheet asset-stencil) index)
  (let ((width (stencil-width sheet))
        (height (stencil-height sheet)))
    (assert (< (* index width height) (length (sheet-data sheet))))
    (let* ((start (* index width height))
           (end (+ start (* width height))))
      (subseq (sheet-data sheet) start end))))

(defmethod initialize-asset ((asset asset-stencil) renderer) ())

(defmethod clear-asset ((asset asset-stencil)) ())

(defclass asset-sprite (asset)
  ((filename :accessor sheet-filename
             :initarg :filename)
   (texture :accessor sheet-texture
            :initarg :texture)
   (texture-backup :accessor sheet-texture-backup)
   (mutablep :accessor sheet-mutablep
             :initarg :mutablep)
   (sprite-width :accessor sheet-sprite-width
                 :initarg :sprite-width)
   (sprite-height :accessor sheet-sprite-height
                  :initarg :sprite-height))
  (:default-initargs :mutablep nil))


(defmethod sheet-columns ((sheet asset-sprite))
  (floor (sdl2:texture-width (sheet-texture sheet))
         (sheet-sprite-width sheet)))

(defmethod sheet-lines ((sheet asset-sprite))
  (floor (sdl2:texture-height (sheet-texture sheet))
         (sheet-sprite-height sheet)))

(defmethod sprite-coords ((sheet asset-sprite) index)
  (let ((ncols (sheet-columns sheet))
        (width (sheet-sprite-width sheet))
        (height (sheet-sprite-height sheet)))
    (multiple-value-bind (y x) (floor index ncols)
      (values (* x width) (* y height)))))

(defun set-surface-transparent-color (surface)
  "Set the color key (transparent pixel) in a surface to black : r=0, g=0, b=0"
  (sdl2:set-color-key surface +sdl-true+ (sdl2:map-rgb (sdl2:surface-format surface) 0 0 0)))

(defun writeable-texture (renderer texture)
  (let ((target (sdl2:create-texture renderer
                                     sdl2:+pixelformat-rgb888+
                                     +sdl-textureaccess-target+
                                     (sdl2:texture-width texture)
                                     (sdl2:texture-height texture))))
    (sdl2:set-render-target renderer target)
    (sdl2:render-copy renderer texture)
    (sdl2:set-render-target renderer nil)
    target))

(defmethod initialize-asset ((asset asset-sprite) renderer)
  (with-surface (surface (sheet-filename asset))
    (set-surface-transparent-color surface)
    (setf (sheet-texture asset)
          (sdl2:create-texture-from-surface renderer surface))

    (when (sheet-mutablep asset)
      (setf (sheet-texture-backup asset) (sheet-texture asset)
            (sheet-texture asset) (writeable-texture renderer (sheet-texture asset))))))

(defun reset-asset (engine key)
  (let ((renderer (engine-renderer engine))
        (asset (find-asset engine key)))
    (when (sheet-mutablep asset)
      (setf (sheet-texture asset) (writeable-texture renderer (sheet-texture-backup asset))))))

(defmethod clear-asset ((asset asset-sprite))
  (sdl2:destroy-texture (sheet-texture asset))
  (when (and (sheet-mutablep asset) (slot-boundp asset 'texture-backup))
    (sdl2:destroy-texture (sheet-texture-backup asset))))


(defmethod add-asset ((engine engine) name filename width height &key (type :sprite) (mutablep nil))
  (let ((assets (engine-assets engine)))
    (case type
      (:sprite (setf (gethash name assets)
                    (make-instance 'asset-sprite :filename filename
                                                 :sprite-width width
                                                 :sprite-height height
                                                 :mutablep mutablep)))
      (:stencil (setf (gethash name assets)
                      (make-instance 'asset-stencil :filename filename
                                                     :stencil-width width
                                                     :stencil-height height))))))


(defmacro with-text-asset ((engine asset name font-file font-size) &body body)
  `(let ((,asset (make-instance 'asset-text
                                :font (make-instance 'font :pathname ,font-file
                                                           :size ,font-size))))
     ,@body
     (setf (gethash ,name (engine-assets ,engine)) ,asset)))


