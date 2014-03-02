(defun pixel-make-palette (&rest colors)
  (list :colors colors))

(defun pixel-normalize-color (type color)
  (if (string-equal type "int")
      (list (/ (float (nth 0 color)) (float 255))
            (/ (float (nth 1 color)) (float 255))
            (/ (float (nth 2 color)) (float 255)))
    color))

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

(provide 'pixel-palette)
