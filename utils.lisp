(in-package :sdl2-engine)

(defmacro with-surface ((surface filename) &body body)
 `(let ((,surface (sdl2-image:load-image ,filename)))
     (unwind-protect
	  ,@body
       (when ,surface
	 (sdl2:free-surface ,surface)))))

(defun load-stencil (filename)
  (uiop:with-input-file (stream filename)
     (let* ((size (file-length stream))
            (bits (make-array size :element-type 'bit)))
      (read-sequence bits stream))))

(defmacro with-rect ((rect x y w h) &body body)
  `(let ((,rect (sdl2:make-rect ,x ,y ,w ,h)))
     (unwind-protect
          ,@body
       (when ,rect
         (sdl2:free-rect ,rect)))))

(defmacro with-ticks ((elapsed delta) &body body)
  (let ((ticks (gensym)))
   `(let ((,ticks (sdl2:get-ticks)))
      (when (>= (- ,ticks ,elapsed) ,delta)
        (unwind-protect
             ,@body
          (setf ,elapsed ,ticks))))))
