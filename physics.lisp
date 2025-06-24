(in-package :sdl2-engine)

(defun sub-rect (x y w h intersect-rect)
  "Return values of the nested rectangle of rect and intersect-rect"
  (let ((xi (sdl2:rect-x intersect-rect))
        (yi (sdl2:rect-y intersect-rect))
        (wi (sdl2:rect-width intersect-rect))
        (hi (sdl2:rect-height intersect-rect)))
    (assert (and (<= wi w) (<= hi h)))
    (values (abs (- xi x)) (abs (- yi y)) wi hi)))

(defun sub-array (array x y w h intersect-rect)
  "Return a sub array of array according intersect-rect
   - array : is an 1 dimensional array computed as it be a 2 dimensional array
     represented by x y w h
   - intersect-rect : A SDL2 rectangle"

  (multiple-value-bind (xs ys ws hs) (sub-rect x y w h intersect-rect)
    (loop with sub-array = (make-array (* ws hs) :element-type 'bit)
          for j integer from 0 below hs
	  do (loop for i integer from 0 below ws
		   do (setf (aref sub-array (+ i (* j ws)))
			    (aref array (+ (* (+ ys j) w) xs i))))
          finally (return sub-array))))


(defun sub-array-index (i x y w ws)
  (declare (type integer i x y w ws))
  (multiple-value-bind (divisor remainder) (truncate i ws)
    (declare (ignore divisor) )
    (+ x 
       (* (+ y (truncate i ws)) w)
       remainder)))

(defun sub-array-one-loop (array x y w h intersect-rect)
  "Return a sub array of array according intersect-rect
   - array : is an 1 dimensional array computed as it be a 2 dimensional array
     represented by x y w h
   - intersect-rect : A SDL2 rectangle.
   ** Speed of sub-array function is best **"
  (declare (type integer x y w h) (type (array bit 1)))
  (multiple-value-bind (xs ys ws hs) (sub-rect x y w h intersect-rect)
    (declare (ignore xs ys))
    (loop with sub-array = (make-array (* ws hs) :element-type 'bit)
          for i integer from 0 below (* ws hs)
          for j integer = (sub-array-index i
                                   (abs (- x (sdl2:rect-x intersect-rect)))
                                   (abs (- y (sdl2:rect-y intersect-rect)))
                                   w
                                   ws)
          do (setf (aref sub-array i) (aref array j))
          finally (return sub-array))))


(defun compare-not-zero-p (array1 array2)
  "Compare 2 arrays of bits :
   - checks that all bits in the result array are zero"
  (assert (= (length array1) (length array2)))
  (not (zerop
        (reduce #'(lambda (i j) (logior i j))
        	(map 'bit-vector #'(lambda (a b) (logand a b)) array1 array2)))))


(let ((rect1 (sdl2:make-rect 0 0 0 0))
      (rect2 (sdl2:make-rect 0 0 0 0)))
  (defun intersectp* (x1 y1 w1 h1 x2 y2 w2 h2)
    (setf (sdl2:rect-x rect1) x1
          (sdl2:rect-y rect1) y1
          (sdl2:rect-width rect1) w1
          (sdl2:rect-height rect1) h1
          (sdl2:rect-x rect2) x2
          (sdl2:rect-y rect2) y2
          (sdl2:rect-width rect2) w2
          (sdl2:rect-height rect2) h2)

    (sdl2:intersect-rect rect1 rect2)))

(defun intersectp2* (x1 y1 w1 h1 x2 y2 w2 h2)
  (with-rect (rect1 x1 y1 w1 h1)
    (with-rect (rect2 x2 y2 w2 h2)
      (sdl2:intersect-rect rect1 rect2))))


(defun intersectp (x1 y1 w1 h1 x2 y2 w2 h2)
  (multiple-value-bind (flag rect) (intersectp* x1 y1 w1 h1 x2 y2 w2 h2)
    (sdl2:free-rect rect)
    flag))

(defun stencils-intersectp-* (stencil1 x1 y1 w1 h1 stencil2 x2 y2 w2 h2)
  "stencils collision predicate function"
  (multiple-value-bind (flag rect) (intersectp2* x1 y1 w1 h1 x2 y2 w2 h2)
    (unwind-protect
         (when flag
           (let ((array1 (sub-array stencil1 x1 y1 w1 h1 rect))
                 (array2 (sub-array stencil2 x2 y2 w2 h2 rect)))
               (compare-not-zero-p array1 array2)))
      (sdl2:free-rect rect))))


(defmethod stencils-intersectp ((engine engine) sheet-name1 index1 x1 y1 sheet-name2 index2 x2 y2)
  (let ((sheet1 (gethash sheet-name1 (engine-assets engine)))
        (sheet2 (gethash sheet-name2 (engine-assets engine))))
    (stencils-intersectp-* (stencils-bits sheet1 index1)
                           x1 y1 (stencil-width sheet1) (stencil-height sheet1)
                           (stencils-bits sheet2 index2)
                           x2 y2 (stencil-width sheet2) (stencil-height sheet2))))
