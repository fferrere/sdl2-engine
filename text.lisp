(in-package :sdl2-engine)

;; With cl-sdl2-ttf You don't have to free surface !

(defclass font ()
  ((pathname :reader font-pathname :initarg :pathname)
   (size :reader font-size :initarg :size)
   (font :accessor font-font :initform nil)))

(defmethod initialize-font ((font font))
  (unless (font-font font)
    (setf (font-font font)
          (sdl2-ttf:open-font (font-pathname font) (font-size font)))))

(defmethod free-font ((font font))
  (sdl2-ttf:close-font (font-font font)))

(defclass text ()
  ((string :reader text-string :initarg :string)
   (red :reader text-red :initarg :red)
   (green :reader text-green :initarg :green)
   (blue :reader text-blue :initarg :blue)
   (alpha :reader text-alpha :initarg :alpha)
   (x :accessor text-x :initarg :x)
   (y :accessor text-y :initarg :y)
   (w :accessor text-w :initarg :w)
   (h :accessor text-h :initarg :h)))

(defmethod setup-text-coords ((text text) x y w h)
  (setf (text-x text) x
        (text-y text) y
        (text-w text) w
        (text-h text) h))

(defmethod text-surface ((text text) font)
  "Return a SDL2 surface."
  (sdl2-ttf:render-utf8-solid (font-font font)
                              (text-string text)
                              (text-red text)
                              (text-green text)
                              (text-blue text)
                              (text-alpha text)))

(defun target-surface (surfaces)
  (let ((depth 32))
    (multiple-value-bind (w h)
        (loop for surface in surfaces
              for s-width = (sdl2:surface-width surface)
              for s-height = (sdl2:surface-height surface)
              maximize s-width into final-width
              sum s-height into final-height
              finally (return (values final-width final-height)))
      (sdl2:create-rgb-surface w h depth))))

(defun number-to-digits (number)
  "Convert an integer to a list of its digits"
  (assert (integerp number))
  (if (= number 0)
      '(0)
      (labels ((collect-digits (numb result)
                 (if (= numb 0)
                     result
                     (multiple-value-bind (divisor remainder) (floor numb 10)
                       (collect-digits divisor (cons remainder result))))))
        (collect-digits number '()))))

(defun draw-text-at (renderer texture text x y &key (position :left))
  (when text
   (with-rect (src-rect (text-x text) (text-y text) (text-w text) (text-h text))
     (case position
       (:center (with-rect (dst-rect (- x (truncate (text-w text) 2))
                                     y
                                     (text-w text) (text-h text))
                  (sdl2:render-copy renderer texture :source-rect src-rect :dest-rect dst-rect)))
       (t (with-rect (dst-rect x y (text-w text) (text-h text))
            (sdl2:render-copy renderer texture :source-rect src-rect :dest-rect dst-rect)))))))

(defun draw-number-at (renderer asset number x y)
  (let ((digits (number-to-digits number))
        (texture (at-texture asset)))    
    (loop for digit in digits
          for text = (gethash (prin1-to-string digit) (at-texts asset))
          for x1 = x then (+ x1 (text-w text))
          do (draw-text-at renderer texture text x1 y))))
