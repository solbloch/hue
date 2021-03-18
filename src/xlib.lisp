;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Xlib colors from display
;;------------------------------------------------------------------------------------------------------------------------ ;;

(in-package :hue)

(defvar host "")

(defvar display (xlib:open-display host))
(defvar screen (first (xlib:display-roots display)))
(defvar root-window (xlib:screen-root screen))

(defmacro with-display (host (display screen root-window) &body body)
  "For when you don't want to keep the xlib-display open"
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defun screen-px-to-hue (window px py)
  (let ((color (car (xlib:query-colors
                   (xlib:window-colormap window)
                   (list (+ px (* py (xlib:screen-width screen))))))))
    (rgb-to-hue (xlib:color-red color)
                (xlib:color-green color)
                (xlib:color-blue color))))

;; Xorg pixelmap uses BGR instead of RGB
(defun grab-rgb-from-window (window x y)
  (unless window (setf window root-window))
  (let* ((image (xlib:get-image window :x x :y y :width 1 :height 1))
         (d (slot-value image 'xlib::data)))
    (list (aref d 2) (aref d 1) (aref d 0))))

(defun grab-from-screen (window x y)
  (unless window (setf window root-window))
  (let* ((image (xlib:get-image window :x x :y y :width 1 :height 1))
         (d (slot-value image 'xlib::data)))
    (rgb-to-hue (aref d 2) (aref d 1) (aref d 0))))
