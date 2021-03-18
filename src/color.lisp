(in-package :hue)

(defun %rgb-to-hue (red green blue)
  (multiple-value-bind (red green blue)
      (values-list (mapcar #'(lambda (x) (/ x 255))
                           (list red green blue)))
    (let* ((max (max red green blue))
        (min (min red green blue))
           (chroma (- max min))
           (brightness (+ (* red .21) (* green .72) (* blue .07)))
           (res))
      (cond ((= red green blue)
             (setf res (* 60 max)))
            ((= red max)
             (setf res (* 60 (/ (- green blue) (- max min)))))
            ((= green max)
             (setf res (* 60 (+ 2 (/ (- blue red) (- max min))))))
            ((= blue max)
             (setf res (* 60 (+ 4 (/ (- red green) (- max min)))))))
      (list res chroma brightness))))

(defun rgb-to-xy (red green blue)
  (labels ((convert (val scale)
             (if (> val .04045)
                 (expt (/ (+ (+ val scale) .055) (+ 1.0 0.055)) 2.4)
                 (/ val 12.92))))
    (let* ((red (convert (/ red 255) .005))
           (green (convert (/ green 255) 0))
           (blue (convert (/ blue 255) 0))
           (X (+ (* red 0.649926) (* green 0.103455) (* blue 0.197109)))
           (Y (+ (* red 0.234327) (* green 0.743075) (* blue 0.022598)))
           (Z (+ (* red 0.0000000) (* green 0.053077) (* blue 1.035763))))
      ;; Y can be used as the brightness value
      (if (zerop (+ X Y Z)) (list 0 0)
          (list (/ X (+ X Y Z)) (/ Y (+ X Y Z)) Y)))))

(defun rgb-to-hue (red green blue)
  (let* ((res (%rgb-to-hue red green blue))
         (v (car res))
         (c (nth 1 res))
         (b (nth 2 res))
         (scale (/ 655.35 360))) ;; Hue offers 65535 colors, unsigned short int
    (list (round (* scale (if (>= v 0) (* 100 v) (* 100 (+ 360 v))))) c b)))

(defun dec-to-hex (dec)
  (write-to-string dec :base 16))

(deftype unit-real ()
  "Real number in [0,1]."
  '(real 0 1))

(defstruct (rgb (:constructor rgb (red green blue)))
  "RGB color."
  (red nil :type unit-real :read-only t)
  (green nil :type unit-real :read-only t)
  (blue nil :type unit-real :read-only t))

(defun parse-hex-rgb (string &key (start 0) end)
  "Parses a hexadecimal RGB(A) color string.  Returns a new RGB color value
and an alpha component if present."
  (let* ((length (length string))
         (end (or end length))
         (sub-length (- end start)))
    (cond
      ;; check for valid range, we need at least three and accept at most
      ;; nine characters
      ((and (<= #.(length "fff") sub-length)
            (<= sub-length #.(length "#ffffff00")))
       (when (char= (char string start) #\#)
         (incf start)
         (decf sub-length))
       (labels ((parse (string index offset)
                  (parse-integer string :start index :end (+ offset index)
                                        :radix 16))
                (short (string index)
                  (/ (parse string index 1) 15))
                (long (string index)
                  (/ (parse string index 2) 255)))
         ;; recognize possible combinations of alpha component and length
         ;; of the rest of the encoded color
         (multiple-value-bind (shortp alphap)
             (case sub-length
               (#.(length "fff") (values T NIL))
               (#.(length "fff0") (values T T))
               (#.(length "ffffff") (values NIL NIL))
               (#.(length "ffffff00") (values NIL T)))
           (if shortp
               (values
                (rgb
                 (short string start)
                 (short string (+ 1 start))
                 (short string (+ 2 start)))
                (and alphap (short string (+ 3 start))))
               (values
                (rgb
                 (long string start)
                 (long string (+ 2 start))
                 (long string (+ 4 start)))
                (and alphap (long string (+ 6 start))))))))
      (T
       (error "not enough or too many characters in indicated sequence: ~A"
              (subseq string start end))))))
