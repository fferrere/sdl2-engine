(in-package :sdl2-engine)

;; API
;; - make-instance 'game
;;   - set handlers
;;   - set window dimensions
;;   - set default draw color
;; - add-asset
;; - add-keyboard mapping
;; - launch

;; Game Engine

(defun nothing-key-handler (keycode engine) ())
(defun nothing-handler (engine) ())

(defclass engine ()
  ((thread :accessor engine-thread)
   (assets :accessor engine-assets :initform (make-hash-table))
   (renderer :accessor engine-renderer)
   (loop :accessor handler-loop :initarg :loop)
   (quit :accessor handler-quit :initarg :quit)
   (keyboard-mapping :accessor keyboard-mapping
                     :initform (make-hash-table :test 'equalp))
   (draw-color :accessor default-draw-color :initarg :draw-color
               :documentation "default draw color : (list r g b a)")
   (width :accessor engine-width :initarg :width)
   (height :accessor engine-height :initarg :height))
  (:default-initargs
   :loop #'nothing-handler
   :quit #'nothing-handler
   :draw-color '(0 0 0 0)
   :width 800
   :height 600))

(defmethod initialize-assets ((engine engine))
  (loop for k being the hash-keys of (engine-assets engine) using (hash-value asset)
        do (initialize-asset asset (engine-renderer engine))))

(defmethod find-asset ((engine engine) key)
  (gethash key (engine-assets engine) nil))

(defmethod clear-assets ((engine engine))
  (loop for k being the hash-keys of (engine-assets engine) using (hash-value asset)
        do (clear-asset asset)))

(defmethod find-text ((engine engine) asset-key id)
  (gethash id (at-texts (find-asset engine asset-key)) nil))


(defmethod kb-event ((engine engine) keysym status)
  (let ((scancode (sdl2:scancode keysym)))
    (when scancode
      (let ((kb (keyboard-binding engine scancode)))
        (when (and kb (key-enabledp kb) (key-handler kb))
              (funcall (key-handler kb) engine status))))))


(defmethod launch ((engine engine) &optional (title "SDL2 Engine"))
  (setf (engine-thread engine)
        (bt:make-thread
         (lambda ()
           (sdl2:with-init (:everything)
             (sdl2:with-window (win :title title :flags '(:shown)
                                    :w (engine-width engine)
                                    :h (engine-height engine))
               (sdl2:with-renderer (renderer win :flags '(:accelerated))
                 (setf (engine-renderer engine) renderer)
                 (let ((last-tick (sdl2:get-ticks)))
                   (sdl2-ttf:init)
                   (initialize-assets engine)
                   
                   (sdl2:with-event-loop (:method :poll)
                     (:keydown (:keysym keysym)
                               (kb-event engine keysym :down))
                     (:keyup (:keysym keysym)
                             (kb-event engine keysym :up)
                             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                               (sdl2:push-event :quit)))
                     (:idle ()
                            (sdl2:render-clear renderer)
                            (apply #'sdl2:set-render-draw-color renderer (default-draw-color engine))
                            (when (handler-loop engine)
                              (funcall (handler-loop engine) engine))
                            (sdl2:render-present renderer)
                            
                            (let ((current-tick (sdl2:get-ticks)))
                              (when (< (- current-tick last-tick) *frame-rate*)
                                (sdl2:delay (- *frame-rate* (- current-tick last-tick))))
                              (setf last-tick current-tick))
                            )
                     (:quit ()
                            (when (handler-quit engine)
                              (funcall (handler-quit engine) engine))
                            (clear-assets engine)
                            (unless (zerop (sdl2-ttf:was-init))
		              (sdl2-ttf:quit))
                            t)))))))
         :name title)))

(defmethod stop ((engine engine))
  (when (sdl2:was-init) (sdl2:push-event :quit))
  (let ((thread (engine-thread engine)))
    (when (bt:thread-alive-p thread) (bt:destroy-thread thread))))


