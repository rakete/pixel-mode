
(require 'cl)
(require 'color)

(require 'pixel-editor)

(defun pixel-index (pic x y)
  (+ (* y (plist-get pic :w)) x))

(defun pixel-ref (pic x y)
  (aref (plist-get pic :bitmap) (pixel-index pic x y)))

(defun pixel-set (pic x y value)
  (aset (plist-get pic :bitmap) (pixel-index pic x y) value)
  pic)

(defun pixel-mapc (f pic)
  (let ((w (cdr (plist-get pic :w)))
        (h (cdr (plist-get pic :h))))
    (loop for x from 0 below w
          do (loop for y from 0 below h
                   do (apply f `(,x ,y ,(pixel-ref pic x y)))))))

;; (pixel-mapc (lambda (x y v) (print `(,x ,y ,v))) (pixel-pic :w 2 :h 2))

(defun* pixel-pic (&key (w 16) (h 16) (background -1) (foreground -1))
  (let* ((bitmap (make-vector (* w h) background)))
    (list :w w
          :h h
          :background background
          :foreground foreground
          :bitmap bitmap)))

;; (let ((foo (pixel-pic)))
;;   (pixel-set foo 2 3 2)
;;   (pixel-ref foo 2 3))

(defun* pixel-diagonals (pic &key (gap 8) (color1 0) (color2 0))
  (let* ((w (plist-get pic :w))
         (h (plist-get pic :h))
         (n (/ w gap)))
    (loop for j from 1 to h
          do (loop for i from 1 to n
                   do (let* ((y (- j 1))
                             (k (- (* i gap) j))
                             (x (if (< k 0) (+ w k) k)))
                        (pixel-set pic x y color1))))
    pic))

;; (print (assoc :bitmap (pixel-diagonals (pixel-pic :w 8 :h 8 :background 0) :gap 4 :color1 1)))

(defun* pixel-edges (pic &key (color1 0) (color2 0) (left t) (top t) (right t) (bottom t))
  (loop for y from 0 below (plist-get pic :h)
        do (loop for x from 0 below (plist-get pic :w)
                 do (when (or (eq y 0)
                              (eq y (- (plist-get pic :h) 1))
                              (eq x 0)
                              (eq x (- (plist-get pic :w) 1)))
                      (pixel-set pic x y color1))))
  pic)

;; (pixel-edges (pixel-pic :w 4 :h 4 :background 0) :color1 1)

(defun* pixel-alpha (pic &key (color -1))
  (let* ((bitmap (make-vector (* (plist-get pic :w) (plist-get pic :h)) 1)))
    (pixel-mapc (lambda (x y v) (when (not (eq v color))
                                  (aset bitmap (pixel-index pic x y) 0)))
                pic)
    (append pic (list :alpha bitmap))))

;; (pixel-alpha (pixel-pic :background 0) :color 0)

;; (defun* pixel-compose (palette &rest pics)
;;   pics)

;; (pixel-compose (pixel-palette "#ffffff" "#000000" "#ff55ff" "#aaaa11")
;;                (pixel-pic :w 16
;;                                :h 16
;;                                :background 3)
;;                (pixel-alpha (pixel-edges (pixel-pic :w 16
;;                                                          :h 16
;;                                                          :background 2)
;;                                          :color1 1
;;                                          :color2 0
;;                                          :left t
;;                                          :top t
;;                                          :right t
;;                                          :bottom t)
;;                             :color 2)
;;                (pixel-alpha (pixel-diagonals (pixel-pic :w 16
;;                                                              :h 16
;;                                                              :background 2)
;;                                              :gap 4
;;                                              :color1 1
;;                                              :color2 0)
;;                             :color 2))

;; (defun pixel-color-decode (str)
;;   (when (and (stringp str) (eq (length str) 7) (eq (elt str 0) ?#))
;;     (let ((r (make-string 2 ?0))
;;           (g (make-string 2 ?0))
;;           (b (make-string 2 ?0)))
;;       (aset r 0 (elt str 1))
;;       (aset r 1 (elt str 2))
;;       (aset g 0 (elt str 3))
;;       (aset g 1 (elt str 4))
;;       (aset b 0 (elt str 5))
;;       (aset b 1 (elt str 6))
;;       `(,(string-to-number r 16)
;;         ,(string-to-number g 16)
;;         ,(string-to-number b 16)))))

;; (defun pixel-color-encode (color)
;;   (format "#%02x%02x%02x" (nth 0 color) (nth 1 color) (nth 2 color)))

(defun pixel-palette (&rest colors)
  (list :colors colors))

(defun pixel-ppm (palette pic)
  (let ((w (plist-get pic :w))
        (h (plist-get pic :h))
        (b (plist-get pic :bitmap))
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

;; (let ((ppm (pixel-ppm (pixel-palette "#000000" "#ffffff" "#ff0000")
;;                       (pixel-edges (pixel-diagonals (pixel-pic :w 16 :h 16 :background 0) :gap 4 :color1 2) :color1 1))))
;;   (loop for n from 0 to 15
;;         do (insert-image (find-image `((:type pbm :data ,ppm :ascent center))))))

(defun pixel-xpm-data (pic)
  (let ((w (plist-get pic :w))
        (h (plist-get pic :h))
        (b (plist-get pic :bitmap))
        (num-colors 0)
        (pixels nil))
    (with-temp-buffer
      (loop for y from 0 below h
            do (progn
                 (insert "\"")
                 (loop for x from 0 below w
                       do (let ((n (pixel-ref pic x y)))
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

;; (let ((data (pixel-xpm-data (pixel-edges (pixel-diagonals (pixel-pic :w 16 :h 16 :background 0) :gap 4 :color1 2) :color1 1)))
;;       (colors (pixel-xpm-colors (pixel-palette "#000000" "#ffffff" "#ff0000"))))
;;   (loop for n from 0 to 0
;;         do (insert-image (find-image `((:type xpm :data ,data
;;                                               :ascent center
;;                                               :color-symbols ,colors))))))

;; (defun* pixel-palette-regex (&key (id nil) (mm nil) (quick nil))
;;   (if id
;;       (setq id (regexp-quote id))
;;     (setq id "[^ \t\n]+"))
;;   (unless mm
;;     (setq mm major-mode))
;;   (cond ((or t ;; REMOVE THIS
;;              (eq mm 'c-mode)
;;              (eq mm 'cc-mode)
;;              (eq mm 'c++-mode))
;;          (if quick
;;              (concat "^[ \t/\*;#%]*\\(?:\\<palette\\|\\<pic\\>\\):[ \t]+\\(" id "\\)\n")
;;            (concat "^[ \t/\*;#%]*\\(?:\\<palette\\>\\|\\<pic\\>\\):[ \t]+\\(" id "\\)\n"
;;                    "\\(\\)" 
;;                    "[^[(\"]*" pixel-c-types-regex "[ \t]+[^[]+\\[\\([0-9]+\\)\\*\\([0-9]+\\)\\(\\)\\].*{\n?"
;;                    "\\(\\([ \t/\*;#%]*\\([0-9\\.]+\\|[0-9]+\\),?[ \t]*\n?\\)+\\).*"
;;                    "};"
;;                    )))))

(defvar pixel-c-types-regex "\\(int\\|bool\\|float\\)")

(defun* pixel-regex (&key (pic nil) (palette nil) (id nil) (mm nil) (quick nil))
  (unless mm
    (setq mm major-mode))
  (unless (or palette pic)
    (setq palette t
          pic t))
  (let* ((palette-id (or (and (stringp palette) palette) "[^ \t\n]+"))
         (init (cond ((or (stringp palette) (and palette pic)) "\\(?:\\<palette\\>\\|\\<pic\\>\\)")
                     (palette "\\<palette\\>")
                     (pic "\\<pic\\>")))
         (using-optional (and (or (not (stringp palette))
                                  (string-equal palette id))
                              "?")))
    (if id
        (setq id (regexp-quote id))
      (setq id "[^ \t\n]+"))
    (cond ((or t ;; REMOVE THIS
               (eql mm 'c-mode)
               (eql mm 'cc-mode)
               (eql mm 'c++-mode))
           (if quick
               (concat "^[ \t/\*;#%]*" init ":[ \t]*\\(" id "\\)\n"
                       "\\(?:[ \t/\*;#%]*using:[ \t]*\\(" palette-id "\\)\n\\)" using-optional)
             (concat "^[ \t/\*;#%]*" init ":[ \t]*\\(" id "\\)\n"
                     "\\(?:[ \t/\*;#%]*using:[ \t]*\\(" palette-id "\\)\n\\)" using-optional
                     "[^[(\"]*" pixel-c-types-regex "[ \t]+[^[]+\\[\\([0-9]+\\)\\*\\([0-9]+\\)\\(?:\\*\\([0-9]+\\)\\)?\\].*{\n?"
                     "\\(\\([ \t/\*;#%]*\\([0-9\\.]+\\|[0-9]+\\),?[ \t]*\n?\\)+\\).*"
                     "};"
                     ))))))

(defun pixel-mapx (x f xs)
  (when (< x 1)
    (setq x 1))
  (let ((steps (ceiling (/ (float (length xs)) (float x)))))
    (loop for n from 0 below steps
          collect (apply f (subseq xs (* n x) (+ (* n x) x))))))

;; (pixel-mapx 3 (lambda (&rest args) (print args)) '(1 2 3 4 5 6 7 8 9 10))

(defun pixel-normalize-color (type color)
  (if (string-equal type "int")
      (list (/ (float (nth 0 color)) (float 255))
            (/ (float (nth 1 color)) (float 255))
            (/ (float (nth 2 color)) (float 255)))
    color))
 
;; (pixel-normalize-color "int" '(255 0 0))

(defun pixel-read-pic-at-point (&optional point)
  (interactive)
  (save-excursion
    (when point
      (goto-char point))
    (when (thing-at-point-looking-at (pixel-regex :pic t :mm major-mode))
      (let* ((id (match-string-no-properties 1))
             (palette-id (or (match-string-no-properties 2) id))
             (type (match-string-no-properties 3))
             (w (read (match-string-no-properties 4)))
             (h (read (match-string-no-properties 5)))
             (c (read (or (match-string-no-properties 6) "1")))
             (xs (read (concat "(" (replace-regexp-in-string "\n\\|;\\|," " " (match-string-no-properties 7)) ")")))
             (d (- (length xs) (* w h c)))
             (palette (when (string-equal id palette-id) (plist-get (plist-get (pixel-read-palette-at-point) :palette) :colors)))
             (bitmap (apply 'vector (pixel-mapx c (lambda (&rest color)
                                                    (cond ((eq (length color) 3)
                                                           (position (apply 'color-rgb-to-hex (pixel-normalize-color type color))
                                                                     palette :test 'equal))
                                                          (t (car color))))
                                                (append xs (make-list d 0))))))
        (list :pic (list :id id
                         :palette-id palette-id
                         :type type
                         :w w
                         :h h
                         :bitmap bitmap)
              :pic-origin (list :id id
                                :beginning (match-beginning 0)
                                :end (match-end 0)
                                :buffer (current-buffer)))))))

(defun pixel-read-palette-at-point (&optional point)
  (interactive)
  (save-excursion
    (when point
      (goto-char point))
    (when (thing-at-point-looking-at (pixel-regex :mm major-mode))
      (let* ((id (match-string-no-properties 1))
             (palette-id (match-string-no-properties 2))
             (type (match-string-no-properties 3))
             (w (read (match-string-no-properties 4)))
             (h (read (match-string-no-properties 5)))
             (c (or (read (or (match-string-no-properties 6) "nil")) w))
             (xs (read (concat "(" (replace-regexp-in-string "\n\\|;\\|," " " (match-string-no-properties 7)) ")")))
             (colors (let ((colors)) (progn
                                       (dotimes (i (* h w) colors)
                                         (let ((r (nth (+ (* i c) 0) xs))
                                               (g (nth (+ (* i c) 1) xs))
                                               (b (nth (+ (* i c) 2) xs)))
                                           (add-to-list 'colors (apply 'color-rgb-to-hex (pixel-normalize-color type (list r g b))) t)
                                           ;; (setq colors (append colors (list (color-rgb-to-hex r g b))))
                                           ))))))
        (list :palette (list :id id
                             :type type
                             :colors colors)
              :palette-origin (list :id
                                    :beginning (match-beginning 0)
                                    :end (match-end 0)
                                    :buffer (current-buffer)))))))

(defun pixel-pic-p (pic)
  (when (and (listp pic)
             (plist-get pic :w)
             (plist-get pic :h)
             (plist-get pic :bitmap)
             (plist-get pic :palette-id))
    pic))

;;(pixel-pic-p (pixel-find-pic :id "test1"))

(defun pixel-origin-p (origin)
  (when (and (listp origin)
             (plist-get origin :beginning)
             (plist-get origin :end)
             (plist-get origin :buffer))
    origin))


;; (defun* pixel-concurrent-list-buffer (&key (with-palette nil) (with-pic nil) (processes 4))
;;   (unless (or with-palette with-pic)
;;     (setq with-palette t
;;           with-pic t))
;;   (let* ((pattern (cond ((and with-palette with-pic)
;;                          "'\\(^[^\"]*palette:[ \\t]*[^ \\t]*\\)\\|\\(^[^\"]*pic:[ \\t]*[^ \\t]*\\)'")
;;                         (with-palette
;;                          "'\\(^[^\"]*palette:[ \\t]*[^ \\t]*\\)'")
;;                         (with-pic
;;                          "'\\(^[^\"]*pic:[ \\t]*[^ \\t]*\\)'")))
;;          (cmd (concat "grep -m 1 " pattern))
;;          (buffers (buffer-list)))
;;     (dolist (buf (buffer-list))
;;       (with-current-buffer buf
;;         (when (and (not buffer-read-only)
;;                    (buffer-file-name (current-buffer)))
;;           (let ((buffer-id (concat "*pixel-proc [" (buffer-name buf) "]*")))
;;             (process-send-region (start-process-shell-command "pixel-grep" buffer-id cmd)
;;                                  (point-min)
;;                                  (point-max))))))))

;; (dolist (buf (buffer-list))
;;   (when (string-match "pixel-proc" (buffer-name buf))
;;     (let ((proc (get-buffer-process buf)))
;;       (when proc
;;         (delete-process proc)))
;;     (kill-buffer buf)))

;; (pixel-concurrent-list-buffer)
;; (pixel-list-buffer)

(defun* pixel-list-buffer ()
  (let ((mm major-mode)
        (result '()))
    (loop for buf in (buffer-list)
          do (pixel-cached (lambda (&rest args)
                             (with-current-buffer buf
                               (when (and (not buffer-read-only)
                                          (buffer-file-name (current-buffer))
                                          ;; (not (eq major-mode 'org-mode))
                                          ;; (or (eq major-mode 'c-mode)
                                          ;;     (eq major-mode 'emacs-lisp-mode))
                                          (save-excursion
                                            (goto-char (point-min))
                                            (let ((case-fold-search nil))
                                              (re-search-forward (pixel-regex :mm major-mode :quick t) nil t))))
                                 (add-to-list 'result buf))))
                           'pixel-buffer-cache
                           :id (buffer-name buf)))
    result))

(defun* pixel-find-palette (&key (id nil) (pic nil) (point nil) (origin nil) (find-origin nil))
  (cond ((numberp point)
         (plist-get (pixel-read-palette-at-point point) (if find-origin :palette-origin :palette)))
        ((pixel-origin-p origin)
         (plist-get (pixel-read-palette-at-point (plist-get origin :beginning)) (if find-origin :palette-origin :palette)))
        ((or (stringp id)
             (and (pixel-pic-p pic) (setq id (plist-get pic :palette-id))))
         (save-excursion
           (goto-char (point-min))
           (let ((case-fold-search nil))
             (if (re-search-forward (pixel-regex :palette id :mm major-mode :id id) nil t)
                 (plist-get (pixel-read-palette-at-point) (if find-origin :palette-origin :palette))
               (let ((buffers (pixel-list-buffer))
                     (result nil))
                 (while (and buffers
                             (not (with-current-buffer (pop buffers)
                                    (save-excursion
                                      (goto-char (point-min))
                                      (when (re-search-forward (pixel-regex :palette id :mm major-mode :id id) nil t)
                                        (setq result (plist-get (pixel-read-palette-at-point) (if find-origin :palette-origin :palette)))))))))
                 result)))))))

;; (pixel-find-palette :pic (pixel-find-pic :id "test1"))

(defun* pixel-find-pic (&key (id nil) (point nil) (origin nil) (find-origin nil))
  (cond ((numberp point)
         (plist-get (pixel-read-pic-at-point point) (if find-origin :pic-origin :pic)))
        ((pixel-origin-p origin)
         (plist-get (pixel-read-pic-at-point (plist-get origin :beginning)) (if find-origin :pic-origin :pic)))
        ((stringp id)
         (save-excursion
           (goto-char (point-min))
           (let ((case-fold-search nil))
             (if (re-search-forward (pixel-regex :pic t :mm major-mode :id id) nil t)
                 (plist-get (pixel-read-pic-at-point) (if find-origin :pic-origin :pic))
               (let ((buffers (pixel-list-buffer))
                     (result nil))
                 (while (and buffers
                             (not (with-current-buffer (pop buffers)
                                    (save-excursion
                                      (goto-char (point-min))
                                      (when (re-search-forward (pixel-regex :pic t :mm major-mode :id id) nil t)
                                        (setq result (plist-get (pixel-read-pic-at-point) (if find-origin :pic-origin :pic)))))))))
                 result)))))))

(defun pixel-canvas-find-free-point (ov)
  (let* ((start (overlay-start ov))
         (end (overlay-end ov))
         (p start))
    (while (and (get-text-property p 'pixel-occupied)
                (< p end))
      (setq p (1+ p)))
    p))

(defun pixel-canvas-insert-tools (canvas &rest tools))

(defun pixel-image (&rest keys)
  (find-image (list keys)))

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

(defun pixel-make-face (id fg &optional hex x y)
  (let* ((sym (intern (cond ((and x y)
                             (format "%s-%s-%d-%d" id (replace-regexp-in-string "#" "" fg) x y))
                            ((stringp hex)
                             (format "%s-%s-%s" id (replace-regexp-in-string "#" "" fg) (replace-regexp-in-string "#" "" hex)))
                            (t
                             (format "%s-%s" id (replace-regexp-in-string "#" "" fg))))))
         (face (make-face sym)))
    (face-spec-reset-face face)
    (modify-face face)
    (unless nil ;;(facep sym)
      (set-face-attribute face nil :box (list :line-width 3 :color fg :style nil)))
    face))

(defun pixel-canvas-insert-palette (canvas palette)
  (when (and canvas palette)
    (save-excursion
      (let* ((ov (plist-get canvas :ov-palette))
             (bg (plist-get canvas :background))
             (fg (plist-get canvas :foreground))
             (rowlength (plist-get canvas :palette-rowlength))
             (rowheight (plist-get canvas :palette-rowheight))
             (indentation (plist-get canvas :indentation))
             (p (pixel-canvas-find-free-point ov))
             (colors (plist-get palette :colors))
             (glyph (pixel-xpm-data (pixel-pic :w rowheight :h rowheight :background 0)))
             (whitespace (pixel-image :type 'xpm
                                      :data (pixel-xpm-data (pixel-pic :w indentation :h rowheight :background 0))
                                      :color-symbols (pixel-xpm-colors (pixel-palette bg bg))
                                      :height rowheight))
             (avg (pixel-palette-average palette))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t)
             (id (plist-get canvas :id)))
        (goto-char p)
        (when (<= (- (point) (point-at-bol)) 0)
          (insert (propertize " "
                              'intangible 'canvas
                              'display whitespace
                              'pixel-occupied t
                              'palette t)))
        (dolist (c colors)
          (when (and rowlength (>= (- (point) (point-at-bol)) (+ (if indentation 1 0) rowlength)))
            (insert (propertize "\n"
                                'intangible 'canvas
                                'pixel-occupied t
                                'pixel-palette t
                                ;;'line-height t
                                ;;'line-spacing nil
                                ))
            (insert (propertize " "
                                'intangible 'canvas
                                'display whitespace
                                'pixel-occupied t
                                'pixel-palette t)))
          (let* ((icon (pixel-image :type 'xpm
                                    :data glyph
                                    :color-symbols (pixel-xpm-colors (pixel-palette c fg))
                                    :height rowheight))
                 (hover-face (pixel-make-face "pixel-mode-palette-hover-face" (color-complement-hex avg) c)))
            (insert (propertize (if (string-equal c (car (last colors)))
                                    (propertize " " 'intangible 'canvas) ;; 
                                  (propertize " "))
                                'display icon
                                'pixel-occupied t
                                'pixel-palette t
                                'mouse-face hover-face
                                ;;'follow-link (pixel-make-palette-action 'mouse1 id c)
                                'keymap (pixel-make-palette-keymap id c)))))))))

(defvar pixel-pixel-cache (make-hash-table :test 'equal))

(defun pixel-make-pixel (type zoomlevel color)
  (let ((key (format "%s-%d-%s" (prin1-to-string type) zoomlevel (replace-regexp-in-string "#" "" color))))
    (cons 'image (cdr (or (gethash key pixel-pixel-cache nil)
                          (puthash key (cond ((eq type 'xpm)
                                              (let ((glyph (pixel-xpm-data (pixel-pic :w (* zoomlevel 2) :h (* zoomlevel 2) :background 0))))
                                                (pixel-image :type 'xpm
                                                             :data glyph
                                                             :color-symbols (pixel-xpm-colors (pixel-palette color))
                                                             :height (* zoomlevel 2)))))
                                   pixel-pixel-cache))))))

(defun pixel-canvas-insert-pic (canvas palette pic)
  (when (and canvas palette pic)
    (save-excursion
      (let* ((ov (plist-get canvas :ov-pic))
             (bg (plist-get canvas :background))
             (zoomlevel (plist-get canvas :zoomlevel))
             (indentation (plist-get canvas :indentation))
             (w (plist-get pic :w))
             (h (plist-get pic :h))
             (p (pixel-canvas-find-free-point ov))
             ;;(glyph (pixel-xpm-data (pixel-pic :w (* zoomlevel 2) :h (* zoomlevel 2) :background 0)))
             (pixel-cache (make-hash-table :test 'equal :size (* w h)))
             (colors (apply 'vector (plist-get palette :colors)))
             (avg (pixel-palette-average palette))
             (whitespace (pixel-image :type 'xpm
                                      :data (pixel-xpm-data (pixel-pic :w indentation :h (* zoomlevel 2) :background 0))
                                      :color-symbols (pixel-xpm-colors (pixel-palette bg))
                                      :height (* zoomlevel 2)))
             (id (plist-get pic :id))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t))
        (goto-char p)
        (dotimes (y h)
          (insert (propertize " "
                              'intangible 'canvas
                              'display whitespace
                              'pixel-occupied t
                              'pixel-pic t))
          (dotimes (x w)
            (let* ((v (pixel-ref pic x y))
                   (c (elt colors v))
                   (pixel (pixel-make-pixel 'xpm zoomlevel c))
                   (hover-face (pixel-make-face "pixel-mode-pic-hover-face" (color-complement-hex avg) nil x y)))
              (insert (propertize (if (eq x (- w 1))
                                      (propertize " " 'intangible 'canvas) 
                                    (propertize " "))
                                  'display pixel
                                  'pixel-occupied t
                                  'pixel-value v
                                  'pixel-color c
                                  'pixel-pic t
                                  'pixel-x x
                                  'pixel-y y
                                  'line-height t
                                  'line-spacing nil
                                  'mouse-face hover-face
                                  ;;'follow-link (pixel-make-pic-action 'mouse1 id x y)
                                  'keymap (pixel-make-pic-keymap id x y)))))
          (insert (propertize "\n"
                              'intangible 'canvas
                              'pixel-occupied t
                              'pixel-pic t
                              'line-height t
                              'line-spacing nil
                              )))))))

;; (setq pixel-canvas-overlays
;;       '(:ov-complete :ov-source :ov-canvas :ov-seperator1 :ov-tools :ov-seperator2 :ov-palette :ov-seperator3 :ov-pic))

;; (setq pixel-canvas-overlays
;;       '(:ov-complete :ov-source :ov-canvas :ov-seperator2 :ov-palette :ov-seperator3 :ov-pic))

(defvar pixel-canvas-overlays
      '(:ov-complete :ov-canvas :ov-seperator2 :ov-palette :ov-seperator3 :ov-pic))

(defun pixel-canvas-remove (canvas)
  (let ((modified-state (buffer-modified-p)))
    (dolist (key pixel-canvas-overlays)
      (when (eq key :ov-canvas)
        (delete-region (overlay-start (plist-get canvas key))
                       (overlay-end (plist-get canvas key))))
      (delete-overlay (plist-get canvas key)))
    (unless (find :ov-source pixel-canvas-overlays)
      (delete-overlay (plist-get canvas :ov-source)))
    (set-buffer-modified-p modified-state)))

;; (pixel-canvas-create (pixel-find-palette :id "bnw") (pixel-find-pic :id "test1") (pixel-find-pic :find-origin t :id "test1")
;;                      :background "#2f2f2f"
;;                      :foreground "#ffffff"
;;                      :source-background "#222222")

(defun* pixel-canvas-create (palette pic origin &key (background nil) (foreground nil) (source-background nil))
  (when (and palette pic origin)
    (unless background
      (setq background (face-attribute 'default :background)))
    (unless foreground
      (setq foreground (face-attribute 'default :foreground)))
    (unless source-background
      (setq source-background background))
    (save-excursion   
      (let* ((first-pos (progn (goto-char (plist-get origin :beginning))
                               (point-at-bol)))
             (src-end (progn (goto-char (plist-get origin :end))
                             (next-line)
                             (point-at-bol)))
             ;; when :ov-source is not in pixel-canvas-overlays, we create a overlay here and hide the text
             (ov-source (unless (find :ov-source pixel-canvas-overlays)
                          (let ((ov (make-overlay first-pos src-end)))
                            (overlay-put ov 'invisible t)
                            ;;(overlay-put ov 'face `((:background "#ff0000")))
                            ov)))
             ;; need overlay symbol so we can add the canvas as property to it
             (ov-complete nil)
             (ov-canvas nil)
             (src-text (unless (overlayp ov-source)
                         (let ((str (buffer-substring first-pos src-end)))
                           (delete-region first-pos src-end)
                           str)))
             (canvas (list :id (plist-get pic :id)
                           :palette-id (plist-get pic :palette-id)
                           :ov-source ov-source
                           :background background
                           :foreground foreground
                           :zoomlevel 12
                           :indentation 20
                           :palette-rowlength 8
                           :palette-rowheight 24))
             (last-pos (if (overlayp ov-source) src-end first-pos))
             (next-pos last-pos)
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t))
        (dolist (key pixel-canvas-overlays)
          (cond ((eq :ov-complete key)
                 (setq ov-complete (make-overlay first-pos (+ last-pos 1)))
                 (plist-put canvas :ov-complete ov-complete))
                ((eq :ov-canvas key)
                 (setq next-pos (progn (goto-char last-pos)
                                       (insert (propertize "\n"
                                                           'intangible 'canvas
                                                           'pixel-overlay key
                                                           ;;'line-height t
                                                           ;;'line-spacing nil
                                                           ))
                                       (point-at-bol)))
                 (setq ov-canvas (make-overlay last-pos next-pos))
                 (overlay-put ov-canvas 'face `((:background ,background)
                                                (:foreground ,foreground)))
                 (plist-put canvas :ov-canvas ov-canvas))
                ((eq :ov-source key)
                 (setq next-pos (progn (goto-char last-pos)
                                       (insert src-text)
                                       (point-at-bol))
                       bg source-background)
                 (setq ov-source (make-overlay last-pos next-pos))
                 (overlay-put ov-source 'face `((:background ,source-background)
                                                (:foreground ,foreground)))
                 (plist-put canvas :ov-source ov-source)
                 (setq last-pos next-pos))
                (t
                 (setq next-pos (progn (goto-char last-pos)
                                       (insert (propertize "\n"
                                                           'intangible 'canvas
                                                           'pixel-overlay key
                                                           ;;'line-height t
                                                           ;;'line-spacing nil
                                                           ))
                                       (point-at-bol)))
                 (let ((ov (make-overlay last-pos next-pos)))
                   (overlay-put ov 'face `((:background ,background)
                                           (:foreground ,foreground)))
                   (plist-put canvas key ov)
                   (setq last-pos next-pos)))))
        (overlay-put ov-complete
                     'pixel-canvas canvas)))))

(defun pixel-match-replace (replacements)
  (save-excursion
    (let ((ovs (mapcar (lambda (r)
                         `(,(make-overlay (match-beginning (car r)) (match-end (car r))) . ,(cdr r)))
                       replacements)))
      (mapc (lambda (ov)
              (let ((o (car ov))
                    (s (cdr ov)))
                (kill-region (overlay-start o) (overlay-end o))
                (goto-char (overlay-start o))
                (insert s)
                (delete-overlay o)))
            ovs))))

(defun* pixel-list-canvas (&key (buffer nil))
  (let ((buffers (if buffer `(,buffer) (pixel-list-buffer)))
        (result '()))
    (loop for buf in buffers
          do (with-current-buffer buf
               (save-excursion
                 (goto-char (point-min))
                 (let ((case-fold-search nil))
                   (while (re-search-forward (pixel-regex :pic t :mm major-mode :quick t) nil t)
                     (let ((overlays (overlays-at (point))))
                       (dolist (ov overlays)
                         (let ((canvas (overlay-get ov 'pixel-canvas)))
                           (when canvas
                             (add-to-list 'result canvas))))))))))
    result))

(defun* pixel-list-pic (&key (buffer nil) (list-origin nil))
  (let ((buffers (if buffer `(,buffer) (pixel-list-buffer)))
        (result '()))
    (loop for buf in buffers
          do (with-current-buffer buf
               (save-excursion
                 (goto-char (point-min))
                 (let ((case-fold-search nil))
                   (while (re-search-forward (pixel-regex :pic t :mm major-mode) nil t)
                     (let ((id (match-string 1)))
                       (add-to-list 'result (if list-origin
                                                (plist-get (pixel-read-pic-at-point) :pic-origin)
                                              (pixel-cached (lambda (&rest args) (plist-get (pixel-read-pic-at-point) :pic))
                                                            'pixel-pic-cache
                                                            :id id))))
                     ;;(add-to-list 'result (plist-get (pixel-read-pic-at-point) (if list-origin :pic-origin :pic)))
                     )))))
    result))

(defun* pixel-find-canvas (&key (id nil) (point nil) (origin nil) (find-origin nil))
  (let ((ovs (cond ((numberp point)
                    (overlays-at point))
                   ((pixel-origin-p origin)
                    (overlays-at (plist-get origin :beginning)))
                   ((stringp id)
                    (let ((origin (pixel-find-pic :find-origin t :id id)))
                      (when (pixel-origin-p origin)
                        (overlays-at (plist-get origin :beginning)))))
                   (t
                    (overlays-at (point)))))
        (canvas nil))
    (while (and ovs (not (setq canvas (overlay-get (pop ovs) 'pixel-canvas)))))
    canvas))

(defun pixel-cached (fn cache &rest args)
  (let ((key (or (plist-get args :id)
                 (plist-get (plist-get args :origin) :id)
                 (plist-get (plist-get args :pic) :palette-id))))
    (or (when (and key (boundp cache) (hash-table-p (eval cache)))
          (or (gethash key (eval cache))
              (puthash key (apply fn args) (eval cache))))
        (apply fn args))))

;; (defun* test-foo (&key (id nil))
;;   23)

;; (let ((bar2 "foo"))
;;   (test-foo nil))

;; (let ((test-cache (make-hash-table :test 'equal)))
;;   (pixel-cached 'test-foo 'test-cache :id "bar")
;;   (print (gethash "bar" test-cache)))

(defun* pixel-toggle-canvas (&key (id nil) (point nil) (origin nil) (remove-active t))
  (interactive)
  (unless (pixel-origin-p origin)
    (setq origin (cond ((stringp id)
                        (pixel-find-pic :id id :find-origin t))
                       ((numberp point)
                        (pixel-find-pic :point point :find-origin t))
                       (pixel-mode
                        (pixel-find-pic :point (point) :find-origin t)))))
  (let ((canvas (pixel-find-canvas :origin origin))
        (modified-state (buffer-modified-p)))
    (when (and canvas remove-active)
      (pixel-canvas-remove canvas))
    (when (not canvas)
      (let* ((pic (pixel-cached 'pixel-find-pic 'pixel-pic-cache :origin origin))
             (palette (pixel-cached 'pixel-find-palette 'pixel-palette-cache :pic pic))
             (canvas (pixel-canvas-create palette pic origin
                                          :background "#2f2f2f"
                                          :foreground "#ffffff"
                                          :source-background "#222222")))
        (pixel-canvas-insert-tools canvas 'pixel 'fill)
        (pixel-canvas-insert-pic canvas palette pic)
        (pixel-canvas-insert-palette canvas palette)
        (set-buffer-modified-p modified-state)))))

(defvar pixel-restore-canvas-after-save-list '())

(defvar pixel-global-pic-cache (make-hash-table :test 'equal))
(defvar pixel-global-palette-cache (make-hash-table :test 'equal))
(defvar pixel-global-buffer-cache (make-hash-table :test 'equal))

(defun pixel-before-save ()
  (let ((pixel-pic-cache pixel-global-pic-cache)
        (pixel-palette-cache pixel-global-palette-cache)
        (pixel-buffer-cache pixel-global-buffer-cache))
    (dolist (canvas (pixel-list-canvas :buffer (current-buffer)))
      ;;(pixel-canvas-save canvas)
      (pixel-canvas-remove canvas)
      (add-to-list 'pixel-restore-canvas-after-save-list (plist-get canvas :id)))))

(defun pixel-after-save ()
  (let ((pixel-pic-cache pixel-global-pic-cache)
        (pixel-palette-cache pixel-global-palette-cache)
        (pixel-buffer-cache pixel-global-buffer-cache))
    (dolist (id pixel-restore-canvas-after-save-list)
      (pixel-toggle-canvas :id id)))
  (setq pixel-restore-canvas-after-save-list '())
  (clrhash pixel-global-pic-cache)
  (clrhash pixel-global-palette-cache)
  (clrhash pixel-global-buffer-cache))

(defadvice save-buffer (around pixel-save-buffer-advice last activate)
  (let ((canvas-list (pixel-list-canvas :buffer (current-buffer))))
    (if (and (buffer-modified-p)
             canvas-list)
        (let ((buffer (get-buffer-create (concat "copy of " (buffer-name)))))
          (copy-to-buffer buffer (point-min) (point-max))
          (dolist (canvas canvas-list)
            (with-current-buffer buffer
              (let ((ov (plist-get canvas :ov-canvas)))
                (delete-region (overlay-start ov) (overlay-end ov)))))
          (let ((visited-file (buffer-file-name)))
            (set-visited-file-name nil t t)
            (with-current-buffer buffer
              (set-visited-file-name visited-file t t)
              ;;(save-buffer)
              ad-do-it
              (let ((process (get-buffer-process (current-buffer))))
                (when process (delete-process process)))
              (kill-buffer))
            (set-visited-file-name visited-file t t)
            (clear-visited-file-modtime)
            (set-buffer-modified-p nil)))
      ;;(save-buffer)
      ad-do-it)))

;;(ad-unadvise 'save-buffer)

;; (ad-add-advice 'save-buffer '(pixel-save-buffer-advice
;;                               nil
;;                               t
;;                               (lambda () (pixel-save-other-buffer)))
;;                'around
;;                'last)
;; (ad-activate 'save-buffer)

(define-minor-mode pixel-mode
  "Create pixel art right inside your programming buffers."
  :lighter " Pixel"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            ;;(define-key map (kbd "C-c C-c") 'pixel)
            map)
  (if (not pixel-mode)
      (progn
        (mapc 'pixel-canvas-remove (pixel-list-canvas :buffer (current-buffer)))
        (remove-hook 'before-save-hook 'pixel-before-save)
        (remove-hook 'after-save-hook 'pixel-after-save))
    (let ((pixel-pic-cache (make-hash-table :test 'equal))
          (pixel-palette-cache (make-hash-table :test 'equal))
          (pixel-buffer-cache (make-hash-table :test 'equal)))
      (add-hook 'before-save-hook 'pixel-before-save)
      (add-hook 'after-save-hook 'pixel-after-save)
      (dolist (origin (pixel-list-pic :buffer (current-buffer) :list-origin t))
        (pixel-toggle-canvas :origin origin)))))
