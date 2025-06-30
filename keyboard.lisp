(in-package :sdl2-engine)

;; sdl2:scancode
;; Converts a keysym to a scancode keyword.

;; sdl2:scancode-value
;; Converts a keysym to the numerical value of its scancode.

;; sdl2:scancode-name
;; Return the string value of its scancode.

;; sdl2:scancode-symbol
;; Converts a scancode number to a scancode keyword.

;; sdl2:scancode-key-to-value
;; Converts a scancode keyword to its numerical value.

;; (defun keycode-keyword (keysym)
;;   (make-keyword
;;    (string-upcase
;;     (sdl2:scancode-name
;;      (sdl2:scancode-value keysym)))))

(defclass key-binding ()
  ((handler :accessor  key-handler :initarg :handler
            :documentation "Keyboard key handler. Args :
                           - Engine : engine object
                           - Status : status of the key - A keyword :up or :down")
   (enabledp :accessor key-enabledp :initarg :enabledp)
   (name :accessor kb-name :initarg :name)
   (key :accessor kb-key :initarg :key)
   (scancode :accessor key-scancode))
  (:default-initargs :enabledp t))


(defmethod add-keyboard-mapping ((engine engine) name key handler)
  (let* ((str (string-downcase (string key)))
         (key-binding (make-instance 'key-binding :name name :key str :handler handler)))
    (setf (gethash str (keyboard-mapping engine)) key-binding)))

(defmethod del-keyboard-mapping ((engine engine) key)
  (remhash (string key) (keyboard-mapping engine)))

(defmethod reset-keyboard-mapping ((engine engine))
  (setf (keyboard-mapping engine) (make-hash-table :test 'equalp)))

(defmethod keyboard-binding ((engine engine) scancode)
  (gethash (sdl2:scancode-name scancode) (keyboard-mapping engine) nil))

(defmethod keyboard-key-string ((engine engine) name)
  (loop for key being the hash-keys of (keyboard-mapping engine)
          using (hash-value key-binding)
        for kb-name = (kb-name key-binding)
        when (string= (kb-name key-binding) name)
          do (return-from keyboard-key-string key)))

(defmethod set-key-status ((engine engine) key status)
  (let ((key-binding (gethash (string key) (keyboard-mapping engine) nil)))
    (when key-binding
      (setf (key-enabledp key-binding) status))))

(defmethod enable-keyboard-key ((engine engine) key)
  (set-key-status engine key t))

(defmethod disable-keyboard-key ((engine engine) key)
  (set-key-status engine key nil))

(defmethod set-keyboard-controls ((engine engine) status)
  (loop for key being the hash-keys of (keyboard-mapping engine) using (hash-value key-binding)
    do (setf (key-enabledp key-binding) status)))

(defmethod enable-keyboard-controls ((engine engine))
  (set-keyboard-controls engine t))

(defmethod disable-keyboard-controls ((engine engine))
  (set-keyboard-controls engine nil))

