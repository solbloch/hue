;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Screen color reader for stumpwm
;;------------------------------------------------------------------------------------------------------------------------ ;;


(defun hue-win-id ()
  (if (current-window) (window-id (current-window))
      (xlib:window-id root-window)))

(defun hue-window ()
  "goes off the root window if there is no current window"
  (if (current-window) (window-xwin (current-window))
      root-window))

(defun find-frame-by-coordinates (posx posy)
  "returns the frame at position x,y; or nil if it can't find one"
  (let* ((frames (group-frames (current-group))))
    (car (remove-if-not #'(lambda (f)
                            (and (and (< (frame-x f) posx) ;; check we're in the right column
                                  (> (+ (frame-x f) (frame-width f)) posx))
                               (and (< (frame-y f) posy) ;; check we're in the right row
                                  (> (+ (frame-y f) (frame-height f)) posy))))
                        frames))))

(defun pos-color (posx posy)
  (let* ((frame (find-frame-by-coordinates posx posy)))
    (if (and frame
           (frame-window frame))
        (grab-rgb-from-window (window-xwin (frame-window frame))
                              (- posx (frame-x frame)) (- posy (frame-y frame)))
        (get-background-color posx posy))))

(defun hue-window ()
  "goes off the root window if there is no current window"
  (if root-window root-window
   (if (current-window) (window-xwin (current-window)))))

(defun hue-window-width ()
  (if (current-window) (window-width (current-window))
      (xlib:screen-width screen)))

(defun hue-window-height ()
  (if (current-window) (window-height (current-window))
      (xlib:screen-height screen)))

;; Calculating the average of rgb colors is a bit counter intuitive. It's not a straight average.
;; but the square root of the average sum of the square of each value.
(defun calculate-average-colors (color-list)
  "Take the square root of the sum of the square of each value"
  (loop for x from 0 to 2
        collect (round (sqrt (mean (mapcar #'(lambda (l)
                                            (expt (nth x l) 2))
                                        color-list))))))

(defun get-average-color-cw ()
  "Grabs the average color of the current window"
  (let* ((tl (pos-color 100 100)) ;; top left
         (bl (pos-color 100 ;; bottom left
                        (- (hue-window-height) 100)))
         (c (pos-color ;; center
                       (round (/ (hue-window-width) 2))
                       (round (/ (hue-window-height) 2))))
         (tr (pos-color ;;top right
                        (- (hue-window-width) 100) 100))
         (br (pos-color ;;bottom right
                        (- (hue-window-width) 100)
                        (- (hue-window-height) 100)))
         (rgb (calculate-average-colors (list tl bl c tr br))))
    (apply #'rgb-to-xy rgb)))

(defun grab-color-range (x y scale)
  "grabs the the color around an x and y area"
  (let ((colors (list (list x y) (list (- x scale) y) (list (+ x scale) y)
                 (list x (- y scale)) (list x (+ y scale)) (list (- x scale) (- y scale))
                 (list (+ x scale) (- y scale)) (list (+ x scale) (+ y scale)))))
    (calculate-average-colors
     (mapcar  #'(lambda (x)
                  (color-by-coordinates (car x) (cadr x)))
              colors))))

(defclass sync-loop ()
  (last-color )
  (average-color (av (get-average-color-hue))
   (color (car av))
   (sat (round (* 255 (* 1.5 (nth 1 av)))))
   (brightness (round (* 255 (* 1.2 (nth 2 av))))))
  )

(defun sync-function ()
  (let* ((av (get-average-color-hue))
         (color (car av))
         (sat (round (* 255 (* 1.5 (nth 1 av)))))
         (brightness (round (* 255 (* 1.2 (nth 2 av))))))
    (unless (equal av last)
      (loop for x in (hash-table-keys *living-room*)
            do (set-light-hue x (if (> brightness 255) 255 brightness)
                              color (if (> sat 255) 255 sat))))
    (setf last av)
    (sleep (* .2 (hash-table-count *living-room*)))))

(defun light-sync-loop ()
  "Define a loop"
  )
