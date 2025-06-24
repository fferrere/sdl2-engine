(in-package :sdl2-engine/demo)

(defvar *source-dir* (asdf:system-source-directory :sdl2-engine/demo))
(defvar *demo-dir* (merge-pathnames #P"demo/" *source-dir*))
(defvar *sprites-filename* (merge-pathnames #P"characters.png" *demo-dir*))
(defvar *stencils-filename* (merge-pathnames #P"stencils.bin" *demo-dir*))
(defvar *font-filename* (merge-pathnames #P"DejaVuSerif-Regular.ttf" *demo-dir*))

(defvar *window-width* 400)
(defvar *window-height* 400)
(defvar *sprite-scale* 4)

(defparameter *player-x* 0)
(defparameter *player-y* 4)
(defparameter *player-indexes* '(:up #(39 40 41) :down #(3 4 5) :left #(15 16 17) :right #(27 28 29)))
(defparameter *player-index* 0)
(defparameter *player-direction* :down)
(defparameter *player-move* nil)
(defparameter *player-fire* nil)
(defparameter *player-animation* nil)

(defparameter *elapsed* 0)
(defparameter *delta* 20)

(defun init ()
  (setf *player-x* 0
        *player-y* 4
        *player-index* 0
        *player-direction* :down
        *player-move* nil
        *plauyer-fire* nil
        *player-animation* nil
        *acc* 0))

(defun move-player (key-status direction)
  (cond ((eql key-status :down) (progn
                                  (setf *player-direction* direction)
                                  (setf *player-move* t)
                                  (setf *player-animation* t)))
        ((eql key-status :up) (progn
                                (setf *player-move* nil)
                                (setf *player-animation* nil)))))

(defun move-left (game key-status)
  (move-player key-status :left))

(defun move-right (game key-status)
  (move-player key-status  :right))

(defun move-up (game key-status)
  (move-player key-status  :up))

(defun move-down (game key-status)
  (move-player key-status  :down))

(defun display-bouding-boxes (game key-status)
  (when (eql key-status :down)
    (if *player-fire*
        (setf *player-fire* nil)
        (setf *player-fire* t))))

(defun compute-move ()
  (when *player-move*
    (case *player-direction*
      (:left (decf *player-x*))
      (:right (incf *player-x*))
      (:up (decf *player-y*))
      (:down (incf *player-y*)))
    (let ((player-width 16)
          (player-height 16))
      (cond
        ((< *player-x* 0) (setf *player-x* 0))
        ((> *player-x* (- (truncate *window-width* *sprite-scale*) player-width))
         (setf *player-x* (- (truncate *window-width* *sprite-scale*) player-width)))
        ((< *player-y* 4) (setf *player-y* 4))
        ((> *player-y* (- (truncate *window-height* *sprite-scale*) player-height))
         (setf *player-y* (- (truncate *window-height* *sprite-scale*) player-height)))))))

(defun player-sprite-index ()
  (aref (getf *player-indexes* *player-direction*) *player-index*))

(defun game-loop (game)
  (let ((x 40) (y 40)
        (index (player-sprite-index)))
    (when *player-animation*
      (incf *player-index*))
    (when (> *player-index* 2)
      (setf *player-index* 0))
    (s2e:with-ticks (*elapsed* *delta*)
     (compute-move))
    (s2e:draw-sprite game :sprites index
                     (* *player-x* *sprite-scale*)
                     (* *player-y* *sprite-scale*)
                          :scale-w *sprite-scale*
                          :scale-h *sprite-scale*)
    (s2e:draw-sprite game :sprites 55
                     (* x *sprite-scale*)
                     (* y *sprite-scale*)
                          :scale-w *sprite-scale*
                          :scale-h *sprite-scale*)
    (when *player-fire*
      (s2e:draw-rect game
                     (* x *sprite-scale*)
                     (* y *sprite-scale*)
                     (* 16 *sprite-scale*)
                     (* 16 *sprite-scale*)
                     :color '(#xFF 0 0 0))
      (s2e:draw-rect game (* *player-x* *sprite-scale*)
                     (* *player-y* *sprite-scale*)
                     (* 16 *sprite-scale*)
                     (* 16 *sprite-scale*)
                     :color '(#xFF 0 0 0)))
    (when (s2e:intersectp *player-x* *player-y* 16 16 x y 16 16)
      (s2e:draw-text game 'text 'contact1 200 0 :position :center))
    (when (s2e::stencils-intersectp game :stencils 0 *player-x* *player-y*
                                         :stencils 1 x y)
      (s2e:draw-text game 'text 'contact2 200 20 :position :center))
    (s2e:draw-text game 'text 'demo1 200 280 :position :center)
    (s2e:draw-text game 'text 'demo2 200 300 :position :center)
    (s2e:draw-text game 'text 'demo3 200 320 :position :center)))

(defun create-stencils-file ()
  (s2e:create-stencils *sprites-filename* '((39 40 41) (55)) 16 16 *stencils-filename*))

(defun start ()
  (let ((game (make-instance 's2e:engine :width *window-width* :height *window-height*
                                         :loop #'game-loop)))
    (s2e:add-asset game :sprites *sprites-filename* 16 16)
    (s2e:add-asset game :stencils *stencils-filename* 16 16 :type :stencil)
    (s2e:add-keyboard-mapping game :left 'move-left)
    (s2e:add-keyboard-mapping game :right 'move-right)
    (s2e:add-keyboard-mapping game :up 'move-up)
    (s2e:add-keyboard-mapping game :down 'move-down)
    (s2e:add-keyboard-mapping game :space 'display-bouding-boxes)
    (s2e:with-text-asset (game asset 'text *font-filename* 16)
      (s2e:add-text asset 'contact1 "BOUNDING BOX CONTACT" #xFF 0 0 0)
      (s2e:add-text asset 'contact2 "STENCILS CONTACT" #xFF #xAA 0 0)
      (s2e:add-text asset 'demo1 "SDL2-ENGINE DEMO" #xF7 #XF7 0 0)
      (s2e:add-text asset 'demo2 "by" #xF7 0 #XF7 0)
      (s2e:add-text asset 'demo3 "Frédéric Ferrère" 0 #x7F #X7F 0)
      (s2e:add-text-digits asset 0 #xf7 0 0))
    (init)
    (s2e:launch game "Demo")))
