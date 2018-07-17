;;; pixel-bitmap.el --- Minor mode for drawing pixel art

;; Copyright (C) 2014-2017 Andreas Raster

;; This file is part of pixel-mode.

;; pixel-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; pixel-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with pixel-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Nothing so far

;;; Code
(require 'color)

(defun pixel-normalize-color (type color)
  "Convert COLOR from TYPE to a float color representation."
  (let ((ret (if (string-equal type "int")
                 (list (/ (float (nth 0 color)) (float 255))
                       (/ (float (nth 1 color)) (float 255))
                       (/ (float (nth 2 color)) (float 255)))
               color)))
    (if (< (length ret) (length color))
        (append ret (last color))
      ret)))

;; (pixel-normalize-color "int" '(255 0 0))

(defun pixel-color-name-to-rgb (type color)
  (if (string-equal type "int")
      (when (> (length color) 6)
        (let ((r (substring color 1 3))
              (g (substring color 3 5))
              (b (substring color 5 7)))
          (list (string-to-number r 16)
                (string-to-number g 16)
                (string-to-number b 16))))
    (color-name-to-rgb color)))

;; (pixel-color-name-to-rgb "float" "#fe9a33")

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

(defun pixel-palette-similar-color (palette color-name)
  (let* ((rgb (color-name-to-rgb color-name))
         (r0 (nth 0 rgb))
         (g0 (nth 1 rgb))
         (b0 (nth 2 rgb))
         (colors (mapcar #'color-name-to-rgb (plist-get palette :colors)))
         (best-distance nil)
         (most-similar (car-safe colors)))
    (dolist (rgb colors (apply #'color-rgb-to-hex most-similar))
      (let* ((r1 (nth 0 rgb))
             (g1 (nth 1 rgb))
             (b1 (nth 2 rgb))
             (dr (- r0 r1))
             (dg (- g0 g1))
             (db (- b0 b1))
             (d (sqrt (+ (* dr dr) (* dg dg) (* db db)))))
        (when (or (not best-distance)
                  (< d best-distance))
          (setq best-distance d
                most-similar rgb))))))

;; (pixel-palette-similar-color (pixel-find-palette :id "Deluxe-Paint-256") "#ffff00")

;; (defun pixel-bitmap-index (bitmap x y &optional c)
;;   (let ((s (plist-get bitmap :stride)))
;;     (+ (+ (* y (plist-get bitmap :width)) (* x s)) (or c 0))))

;; (defun pixel-bitmap-ref (bitmap x y &optional c)
;;   (aref (plist-get bitmap :array) (pixel-bitmap-index bitmap x y c)))

(defun pixel-bitmap-index (bitmap x y)
  (+ (* y (or (and (listp bitmap) (plist-get bitmap :width))
              (and (numberp bitmap) bitmap)))
     x))

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

(defun* pixel-make-bitmap (&key (width 16) (height 16))
  (let* ((array (make-vector (* width height) 0)))
    (list :width width
          :height height
          :array array
          :format "palette"
          :comma " "
          :open "["
          :close "]"
          :type "int")))

;; (let ((foo (pixel-make-bitmap)))
;;   (pixel-bitmap-set foo 2 3 2)
;;   (pixel-bitmap-ref foo 2 3))

(defun* pixel-bitmap-set-diagonals (bitmap &key (gap 8) (color1 0) (color2 0))
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

;; (print (assoc :array (pixel-bitmap-set-diagonals (pixel-make-bitmap :width 8 :h 8 :background 0) :gap 4 :color1 1)))

(defun* pixel-bitmap-set-edges (bitmap &key (color1 0) (color2 0) (left t) (top t) (right t) (bottom t))
  (loop for y from 0 below (plist-get bitmap :height)
        do (loop for x from 0 below (plist-get bitmap :width)
                 do (when (or (eq y 0)
                              (eq y (- (plist-get bitmap :height) 1))
                              (eq x 0)
                              (eq x (- (plist-get bitmap :width) 1)))
                      (pixel-bitmap-set bitmap x y color1))))
  bitmap)

;; (pixel-bitmap-set-edges (pixel-make-bitmap :width 4 :h 4 :background 0) :color1 1)

(defun* pixel-bitmap-set-alpha (bitmap &key (color -1))
  (let* ((array (make-vector (* (plist-get bitmap :width) (plist-get bitmap :h)) 1)))
    (pixel-bitmap-mapc (lambda (x y v) (when (not (eq v color))
                                         (aset array (pixel-bitmap-index bitmap x y) 0)))
                       bitmap)
    (append bitmap (list :alpha array))))

;; (pixel-bitmap-set-alpha (pixel-make-bitmap :background 0) :color 0)

(defun pixel-bitmap-resize (bitmap add-width add-height)
  (let* ((width (plist-get bitmap :width))
         (height (plist-get bitmap :height))
         (new-width (+ width add-width))
         (new-height (+ height add-height))
         (new-array (make-vector (* new-width new-height) 0))
         (new-bitmap (cl-copy-list bitmap)))
    (plist-put new-bitmap :width new-width)
    (plist-put new-bitmap :height new-height)
    (dotimes (x new-width)
      (dotimes (y new-height)
        (when (and (< x width) (< y height))
          (aset new-array
                (pixel-bitmap-index new-bitmap x y)
                (pixel-bitmap-ref bitmap x y)))))
    (plist-put new-bitmap :array new-array)))

;; (print (pixel-bitmap-resize (pixel-make-bitmap :width 1 :height 1) 1 0))

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
;;                       (pixel-bitmap-set-edges (pixel-bitmap-set-diagonals (pixel-make-bitmap :width 16 :h 16 :background 0) :gap 4 :color1 2) :color1 1))))
;;   (loop for n from 0 to 15
;;         do (insert-image (find-image `((:type pbm :data ,ppm :ascent center))))))

(defun pixel-xpm-data (bitmap)
  (let ((w (plist-get bitmap :width))
        (h (plist-get bitmap :height))
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

;; (let ((data (pixel-xpm-data (pixel-bitmap-set-edges (pixel-bitmap-set-diagonals (pixel-make-bitmap :width 16 :h 16 :background 0) :gap 4 :color1 2) :color1 1)))
;;       (colors (pixel-xpm-colors (pixel-make-palette "#000000" "#ffffff" "#ff0000"))))
;;   (loop for n from 0 to 0
;;         do (insert-image (find-image `((:type xpm :data ,data
;;                                               :ascent center
;;                                               :color-symbols ,colors))))))

(provide 'pixel-bitmap)

;;; pixel-bitmap.el ends here
