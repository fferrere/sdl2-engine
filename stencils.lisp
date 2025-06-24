(in-package :sdl2-engine)

;; ------------------------
;; -- Load Stencils file --
;; ------------------------

(defparameter *stencil-viewer* nil)
(defparameter *stencil-current-index* 0)

(defun load-stencils (stencils-file)
  (with-open-file (stream stencils-file :element-type '(unsigned-byte 8))
    (let ((stencils (make-array (* 8 (file-length stream)) :element-type 'bit))
	  (index 0))
      (loop for byte = (read-byte stream nil nil)
            for index = 0 then (+ index 8)
	    while byte
	    do (loop for vb-index from 0 below 8
		     with vb = (if (= byte 0) #*00000000 (bit-smasher:int->bits byte))
                     do (setf (aref stencils (+ index vb-index)) (aref vb vb-index))))
      stencils)))


(defun stencil-to-sprite (renderer stencil x y width height)
  (sdl2:set-render-draw-color renderer #xFF #xFF #xFF 0)
  (loop for i from 0 below (length stencil)
        for yi = (+ y (truncate i width))
        for xi = (+ x (mod i width))
        for bit = (aref stencil i)
        when (not (zerop bit))
          do (sdl2:render-draw-point renderer xi yi))
  (sdl2:set-render-draw-color renderer 0 0 0 0))

(defun sloop (engine)
  (stencil-to-sprite (engine-renderer engine)
                     (stencils-bits (find-asset engine :stencils) *stencil-current-index*)
                     0 0 48 48))

(defun view-stencils (stencils-filename)
    (let ((viewer (make-instance 'engine :loop 'sloop :width 800 :height 700)))
      (add-asset viewer :stencils stencils-filename 48 48 :type :stencil)
      (setf *stencil-viewer* viewer)
      (launch viewer "Stencils Viewer")))

;; ------------------------
;; --  Create Stencils   --
;; ------------------------

(defun pixels-overlay (method &rest sprites-pixels)
  ;; all sprite-pixels must have the same dimensions !!!
  (assert (member method '(logior logand)))
  (apply #'map 'bit-vector
         #'(lambda (&rest numbers)
             (if (zerop (apply #'funcall method numbers)) 0 1))
         sprites-pixels))

(defun create-stencil (surface-sheet indexes sprite-w sprite-h
                         &key (bit-mapping-fn 'logior)
                           (transparent-color '(0 0 0)))
  (apply #'pixels-overlay bit-mapping-fn
	 (mapcar #'(lambda (index)
                     (sprite-pixels-from surface-sheet index sprite-w sprite-h
                                         :transparent-color transparent-color))
		 indexes)))


(defun write-stencils-to-binary-file (stencils filename)
  (with-open-file (stream filename :direction :output
				   :if-exists :supersede
				   :element-type '(unsigned-byte 8))
    (dolist  (stencil stencils)
      (loop for i from 0 below (length stencil) by 8
            for byte = (bit-smasher:bits->int (subseq stencil i (+ i 8)))
            do (write-byte byte stream)))))


(defun create-stencils (sprites-filename list-of-indexes width height stencils-filename)
  (let ((e (make-instance 'engine :width width :height height)))
    (launch e "Stencils Creator")
    (with-surface (surface sprites-filename)
      (write-stencils-to-binary-file
       (mapcar #'(lambda (indexes)
                   (create-stencil surface indexes width height))
               list-of-indexes)
       stencils-filename))
    (stop e)))
