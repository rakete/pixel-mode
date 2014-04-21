(defun pixel-make-palette (&rest colors)
  (list :colors colors))

(defun pixel-normalize-color (type color)
  (let ((ret (if (string-equal type "int")
                 (list (/ (float (nth 0 color)) (float 255))
                       (/ (float (nth 1 color)) (float 255))
                       (/ (float (nth 2 color)) (float 255)))
               color)))
    (if (< (length ret) (length color))
        (append ret (last color))
      ret)))

;; (pixel-normalize-color "int" '(255 0 0))

(defun pixel-palette-average (palette)
  (let* ((ra 0.0)
         (ga 0.0)
         (ba 0.0)
         (colors (plist-get palette :colors))
         (size (float (length colors))))
    (loop for hex in colors
          do (let* ((c (color-name-to-rgb hex))
                    (r (nth 0 c))
                    (g (nth 1 c))
                    (b (nth 2 c)))
               (setq ra (+ ra r)
                     ga (+ ga g)
                     ba (+ ba b))))
    (apply 'color-rgb-to-hex (list (/ ra size) (/ ga size) (/ ba size)))))

;;(pixel-palette-average (pixel-find-palette :id "bnw"))

;; (defun pixel-bitmap-index (bitmap x y &optional c)
;;   (let ((s (plist-get bitmap :stride)))
;;     (+ (+ (* y (plist-get bitmap :width)) (* x s)) (or c 0))))

;; (defun pixel-bitmap-ref (bitmap x y &optional c)
;;   (aref (plist-get bitmap :array) (pixel-bitmap-index bitmap x y c)))

(defun pixel-bitmap-index (bitmap x y)
  (+ (* y (plist-get bitmap :width)) x))

(defun pixel-bitmap-ref (bitmap x y)
  (aref (plist-get bitmap :array) (pixel-bitmap-index bitmap x y)))

;; (defun pixel-bitmap-color (bitmap x y palette)
;;   (let ((format (plist-get bitmap :format))
;;         (colors (plist-get palette :colors)))
;;     (cond ((string-equal format "palette")
;;            (elt colors (pixel-bitmap-ref bitmap x y)))
;;           ((or (string-equal format "rgb") (string-equal format "rgba"))
;;            (apply 'color-rgb-to-hex (loop for c from 0 below 3 collect (pixel-bitmap-ref bitmap x y c)))))))

;; (defun pixel-bitmap-set (bitmap x y value)
;;   (let ((value (if (listp value value (list value))))
;;         (n (car (sort (list (length value) (plist-get bitmap :stride)) #'<))))
;;     (dotimes (c n)
;;       (aset (plist-get bitmap :array) (pixel-bitmap-index bitmap x y c) (nth c value))))
;;   bitmap)

(defun pixel-bitmap-set (bitmap x y value)
  (aset (plist-get bitmap :array) (pixel-bitmap-index bitmap x y) value)
  bitmap)

;; (defun pixel-bitmap-mapc (f bitmap)
;;   (let ((w (plist-get bitmap :width))
;;         (h (plist-get bitmap :height))
;;         (s (plist-get bitmap :stride)))
;;     (loop for x from 0 below w
;;           do (loop for y from 0 below h
;;                    do (if (> s 1)
;;                           (apply f `(,x ,y ,(pixel-bitmap-ref bitmap x y)))
;;                         (apply f `(,x ,y ,(loop for c from 0 below s
;;                                                 collect (pixel-bitmap-ref bitmap x y s)))))))))

(defun pixel-bitmap-mapc (f bitmap)
  (let ((w (cdr (plist-get bitmap :width)))
        (h (cdr (plist-get bitmap :height))))
    (loop for x from 0 below w
          do (loop for y from 0 below h
                   do (apply f `(,x ,y ,(pixel-bitmap-ref bitmap x y)))))))

;; (pixel-bitmap-mapc (lambda (x y v) (print `(,x ,y ,v))) (pixel-make-bitmap :width 2 :h 2))

(defun* pixel-make-bitmap (&key (width 16) (height 16) (background -1) (foreground -1))
  (let* ((array (make-vector (* width height) background)))
    (list :width width
          :height height
          :background background
          :foreground foreground
          :array array)))

;; (let ((foo (pixel-make-bitmap)))
;;   (pixel-bitmap-set foo 2 3 2)
;;   (pixel-bitmap-ref foo 2 3))

(defun* pixel-bitmap-diagonals (bitmap &key (gap 8) (color1 0) (color2 0))
  (let* ((w (plist-get bitmap :width))
         (h (plist-get bitmap :height))
         (n (/ w gap)))
    (loop for j from 1 to h
          do (loop for i from 1 to n
                   do (let* ((y (- j 1))
                             (k (- (* i gap) j))
                             (x (if (< k 0) (+ w k) k)))
                        (pixel-bitmap-set bitmap x y color1))))
    bitmap))

;; (print (assoc :array (pixel-bitmap-diagonals (pixel-make-bitmap :width 8 :h 8 :background 0) :gap 4 :color1 1)))

(defun* pixel-bitmap-edges (bitmap &key (color1 0) (color2 0) (left t) (top t) (right t) (bottom t))
  (loop for y from 0 below (plist-get bitmap :height)
        do (loop for x from 0 below (plist-get bitmap :width)
                 do (when (or (eq y 0)
                              (eq y (- (plist-get bitmap :height) 1))
                              (eq x 0)
                              (eq x (- (plist-get bitmap :width) 1)))
                      (pixel-bitmap-set bitmap x y color1))))
  bitmap)

;; (pixel-bitmap-edges (pixel-make-bitmap :width 4 :h 4 :background 0) :color1 1)

(defun* pixel-bitmap-alpha (bitmap &key (color -1))
  (let* ((array (make-vector (* (plist-get bitmap :width) (plist-get bitmap :h)) 1)))
    (pixel-bitmap-mapc (lambda (x y v) (when (not (eq v color))
                                         (aset array (pixel-bitmap-index bitmap x y) 0)))
                       bitmap)
    (append bitmap (list :alpha array))))

;; (pixel-bitmap-alpha (pixel-make-bitmap :background 0) :color 0)

(defun pixel-bitmap-p (bitmap)
  (when (and (listp bitmap)
             (plist-get bitmap :width)
             (plist-get bitmap :height)
             (plist-get bitmap :array)
             (plist-get bitmap :palette-id))
    bitmap))

;;(pixel-bitmap-p (pixel-find-bitmap :id "test1"))

(defun pixel-ppm (palette bitmap)
  (let ((w (plist-get bitmap :width))
        (h (plist-get bitmap :height))
        (b (plist-get bitmap :array))
        (v (apply 'vector (mapcar (lambda (str)
                                    (let ((c (color-name-to-rgb str)))
                                      (concat (prin1-to-string (* 255 (nth 0 c))) " "
                                              (prin1-to-string (* 255 (nth 1 c))) " "
                                              (prin1-to-string (* 255 (nth 2 c))) " ")))
                                  (plist-get palette :colors)))))
    (with-temp-buffer
      (insert "P3" "\n"
              (prin1-to-string w) " " (prin1-to-string h) "\n"
              "255" "\n")
      (loop for n across b
            do (insert (elt v n)))
      (buffer-string))))

;; (let ((ppm (pixel-ppm (pixel-make-palette "#000000" "#ffffff" "#ff0000")
;;                       (pixel-bitmap-edges (pixel-bitmap-diagonals (pixel-make-bitmap :width 16 :h 16 :background 0) :gap 4 :color1 2) :color1 1))))
;;   (loop for n from 0 to 15
;;         do (insert-image (find-image `((:type pbm :data ,ppm :ascent center))))))

(defun pixel-xpm-data (bitmap)
  (let ((w (plist-get bitmap :width))
        (h (plist-get bitmap :height))
        (b (plist-get bitmap :array))
        (num-colors 0)
        (pixels nil))
    (with-temp-buffer
      (loop for y from 0 below h
            do (progn
                 (insert "\"")
                 (loop for x from 0 below w
                       do (let ((n (pixel-bitmap-ref bitmap x y)))
                            (when (> n num-colors) (setq num-colors n))
                            (insert (format "%04d" n))))
                 (insert "\"" (if (eq y (- h 1)) "};" ",\n"))))
      (setq pixels (buffer-string)))
    (setq num-colors (+ num-colors 1))
    (with-temp-buffer
      (insert "/* XPM */" "\n"
              "static char *noname[] = {" "\n"
              "/* width height ncolors chars_per_pixel */" "\n"
              "\"" (prin1-to-string w) " " (prin1-to-string h) " " (prin1-to-string num-colors) " 4\"," "\n"
              "/* colors */" "\n")
      (loop for n from 0 below num-colors
            do (insert (concat "\"" (format "%04d" n) " s col" (prin1-to-string n) "\"," (unless (eq n (- num-colors 1)) "\n"))))
      (insert "\n"
              "/* pixels */" "\n"
              pixels)
      (buffer-string))))

(defun pixel-xpm-colors (palette)
  (let ((n -1))
    (mapcar (lambda (c)
              (setq n (+ n 1))
              `(,(concat "col" (prin1-to-string n)) . ,c))
            (plist-get palette :colors))))

;; (let ((data (pixel-xpm-data (pixel-bitmap-edges (pixel-bitmap-diagonals (pixel-make-bitmap :width 16 :h 16 :background 0) :gap 4 :color1 2) :color1 1)))
;;       (colors (pixel-xpm-colors (pixel-make-palette "#000000" "#ffffff" "#ff0000"))))
;;   (loop for n from 0 to 0
;;         do (insert-image (find-image `((:type xpm :data ,data
;;                                               :ascent center
;;                                               :color-symbols ,colors))))))

(provide 'pixel-bitmap)
