(in-package :sdl2-engine/test)

(defparameter *img-dir* (asdf:system-relative-pathname :sdl2-engine "img"
                                                       :type :directory))

(define-test stencils
  ;; Pour les tests
  (false (sdl2-engine::compare-not-zero-p
          (make-array 6 :element-type 'bit :initial-contents '(0 0 0 1 0 0))
          (make-array 6 :element-type 'bit :initial-contents '(0 0 1 0 0 0))))
  
  (true (sdl2-engine::compare-not-zero-p
         (make-array 6 :element-type 'bit :initial-contents '(0 0 0 1 0 0))
         (make-array 6 :element-type 'bit :initial-contents '(0 0 0 1 0 0))))
  
  (let ((s1 (make-array 6 :element-type 'bit :initial-contents '(1 1 0 0 0 0)))
        (s2 (make-array 6 :element-type 'bit :initial-contents '(0 0 1 0 0 0))))
    (true (sdl2-engine::stencils-intersectp-* s1 1 0 2 3 s2 0 0 3 2))
    (false (sdl2-engine::stencils-intersectp-* s1 0 0 2 3 s2 0 0 3 2))))


                                                     
(let ((tests-stencils '()))
 (defun load-tests-stencils ()
   (let ((filename1 (merge-pathnames "stencil1.png" *img-dir*))
         (filename2 (merge-pathnames "stencil2.png" *img-dir*))
         (filename3 (merge-pathnames "stencil-or.png" *img-dir*))
         (filename4 (merge-pathnames "stencil-and.png" *img-dir*))
         (width 64)
         (height 64)
         (index 0))
     (unless tests-stencils
       (sdl2:with-init (:everything)
        (sdl2-engine::with-surface (surface1 filename1)
          (sdl2-engine::with-surface (surface2 filename2)
            (sdl2-engine::with-surface (surface3 filename3)
              (sdl2-engine::with-surface (surface4 filename4)
                (setf tests-stencils
                      (list
                       (sdl2-engine::create-stencil-logand
                        (sdl2-engine::sprite-pixels-from surface1 index width height)
                        (sdl2-engine::sprite-pixels-from surface2 index width height))
                       (sdl2-engine::create-stencil-logand
                        (sdl2-engine::sprite-pixels-from surface4 index width height))
                       (sdl2-engine::create-stencil-logior
                        (sdl2-engine::sprite-pixels-from surface1 index width height)
                        (sdl2-engine::sprite-pixels-from surface2 index width height))
                       (sdl2-engine::create-stencil-logior
                        (sdl2-engine::sprite-pixels-from surface3 index width height))
                       (sdl2-engine::create-stencil-logand
                        (sdl2-engine::sprite-pixels-from surface1 index width height))
                       (sdl2-engine::create-stencil-logand
                        (sdl2-engine::sprite-pixels-from surface2 index width height)))))))))))
   tests-stencils))

(define-test compare-stencils-tests
  :description "Load Images - creates stencils and compare"
  (let ((stencils (load-tests-stencils)))
    (is equalp (first stencils) (second stencils))
    (is equalp (third stencils) (fourth stencils))))

(define-test check-collision-tests
  :description "Check collision with stencils"
  (let ((stencils (load-tests-stencils)))
                
    (false (sdl2-engine::stencils-intersectp-*
            (nth 4 stencils) 0 0 64 64
            (nth 5 stencils) 60 20 64 64))

    (false (sdl2-engine::stencils-intersectp-*
            (nth 4 stencils) 0 0 64 64
            (nth 5 stencils) 0 60 64 64))

    (true (sdl2-engine::stencils-intersectp-*
           (nth 4 stencils) 0 0 64 64
           (nth 5 stencils) 40 0 64 64))

    (true (sdl2-engine::stencils-intersectp-*
           (nth 4 stencils) 0 0 64 64
           (nth 5 stencils) 0 0 64 64))

    ))
