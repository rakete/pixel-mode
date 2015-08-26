;; -*- lexical-binding: t -*-

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(require 'pixel-bitmap nil 'noerror)

;;
;; Utilities
;;

(defun* pixel-find-index (x xs &key (test 'eq))
  (let ((n 0)
        (found nil))
    (while (and (< n (length xs))
                (not found))
      (setq found (funcall test (nth n xs) x))
      (unless found
        (setq n (1+ n))))
    (when found n)))

;; (pixel-find-index "foo" '("bar" "foo") :test 'string-equal)

(defun pixel-make-image (&rest keys)
  (find-image (list keys)))

(defvar pixel-hover-face-cache (make-hash-table :test 'equal))

(defun pixel-make-hover-face (id x y)
  (let ((name (format "%s|%d|%d" id x y)))
    (or (gethash name pixel-hover-face-cache nil)
        (puthash name
                 (let* ((sym (intern name))
                        (face (make-face sym)))
                   (face-spec-reset-face face)
                   (set-face-attribute face nil :box (list :line-width 3 :color "#ffffff" :style nil))
                   face)
                 pixel-hover-face-cache))))

;; (defun pixel-copy-hover-face (face &optional hex x y)
;;   (print (face-attribute face :box t 'default))
;;   (let* ((sym (intern (cond ((and x y)
;;                              (replace-regexp-in-string "^.*\\(|[0-9]+|[0-9]+\\)$" (concat "|" (prin1-to-string x) "|" (prin1-to-string y)) (prin1-to-string face) nil nil 1))
;;                             ((stringp hex)
;;                              (replace-regexp-in-string "^.*\\(|[0-9a-fA-F]+\\)$" (concat "|" (replace-regexp-in-string "#" "" hex)) (prin1-to-string face) nil nil 1)))))
;;          (new-face (make-face sym)))
;;     (set-face-attribute new-face nil :box (print (face-attribute face :box t 'default)))
;;     new-face))

(defvar pixel-pixel-cache (make-hash-table :test 'equal))

(defun pixel-make-pixel (color width &optional height)
  (let ((key (format "%d%d%s" width (or height width) color)))
    (append (list 'image) (cdr (or (gethash key pixel-pixel-cache nil)
                                   (puthash key (let ((template (pixel-xpm-data (pixel-make-bitmap :width width :height (or height width)))))
                                         (pixel-make-image :type 'xpm
                                                           :data template
                                                           :color-symbols `(("col0"  . ,color))
                                                           :height (or height width)))
                                            pixel-pixel-cache))))))

;; (defun pixel-map-text-properties (start end props f &optional buffer)
;;   (with-current-buffer (or buffer (current-buffer))
;;     (save-excursion
;;       (goto-char start)
;;       (while (< (point) end)
;;         (forward-char)
;;         (let ((plist '()))
;;           (dolist (prop props)
;;             (setq plist (plist-put plist prop (get-text-property (point) prop))))
;;           (when (every #'identity plist)
;;             (setq plist (funcall f (point) plist))
;;             (add-text-properties (point) (1+ (point)) plist)))))))

;;
;; State
;;
(setq pixel-editor-state (make-hash-table :test 'equal))

(defun pixel-editor-put (editor prop &optional val)
  (or (and (plist-get editor prop)
           (plist-put editor prop val))
      (let* ((key (plist-get editor :id)))
        (when key
          (if prop
              (puthash key (plist-put (or (gethash key pixel-editor-state)
                                          (puthash key '() pixel-editor-state)) prop val)
                       pixel-editor-state)
            (puthash key val pixel-editor-state))
          editor))))

;; (pixel-editor-put (list :id "aaa" :foo "lala") :ficken "bar")

(defun pixel-editor-update (editor prop val)
  (let* ((ov (pixel-editor-get editor :ov-editor))
         (start (overlay-start ov))
         (end (overlay-end ov))
         (pos (1- start)))
    (while (setq pos (text-property-any (1+ pos) end 'pixel-property prop))
      (let ((update-fn (get-text-property pos 'pixel-update)))
        (when (functionp update-fn)
          (add-text-properties pos (1+ pos) (funcall update-fn val)))))))

(defun pixel-editor-get (editor &optional prop default)
  (or (plist-get editor prop)
      (let* ((key (plist-get editor :id)))
        (when key
          (or (unless prop (gethash key pixel-editor-state))
              (plist-get (or (gethash key pixel-editor-state)
                             (puthash key '() pixel-editor-state)) prop))))
      default))

;; (pixel-editor-get (list :id "aaa" :foo "lala") :ficken)

;;
;; Editor
;;

(defun pixel-editor-buffer (editor)
  (overlay-buffer (pixel-editor-get editor :ov-complete)))

(defun pixel-editor-insert-toolbar (editor)
  (when editor
    (save-excursion
      (let* ((id (pixel-editor-get editor :id))
             (ov (pixel-editor-get editor :ov-toolbar))
             (start (overlay-start ov))
             (editor-background (pixel-editor-get editor :editor-background))
             (palette-foreground (pixel-editor-get editor :palette-foreground))
             (palette-background (pixel-editor-get editor :palette-background))
             (indentation (pixel-editor-get editor :editor-indentation))
             (rowheight (pixel-editor-get editor :toolbar-rowheight))
             (whitespace (pixel-make-pixel editor-background indentation rowheight))
             (foreground-icon (pixel-make-pixel palette-foreground rowheight))
             (background-icon (pixel-make-pixel palette-background rowheight))
             (editor-keymap (pixel-make-editor-keymap editor (make-sparse-keymap)))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t))
        (pixel-editor-put editor :tool-current 'pixel-tool-draw)
        (goto-char start)
        (insert (propertize " "
                            'intangible 'editor
                            'display whitespace
                            'pixel-occupied id
                            'pixel-toolbar t
                            'keymap editor-keymap))
        (insert (propertize " "
                            'intangible 'editor
                            'display foreground-icon
                            'pixel-occupied id
                            'pixel-property :palette-foreground
                            'pixel-update (lambda (color)
                                            (let ((icon (pixel-make-pixel color rowheight)))
                                              (list 'display icon)))
                            'pixel-toolbar t
                            'keymap editor-keymap))
        (insert (propertize " "
                            'intangible 'editor
                            'display background-icon
                            'pixel-occupied id
                            'pixel-property :palette-background
                            'pixel-update (lambda (color)
                                            (let ((icon (pixel-make-pixel color rowheight)))
                                              (list 'display icon)))
                            'pixel-toolbar t
                            'keymap editor-keymap))
        (insert (propertize " "
                            'intangible 'editor
                            'display whitespace
                            'pixel-occupied id
                            'pixel-toolbar t
                            'keymap editor-keymap))
        (let ((shortcuts (pixel-editor-get editor :palette-shortcuts)))
          (dotimes (n (length shortcuts))
            (let* ((c (nth n shortcuts))
                   (shortcut-icon (pixel-make-pixel c rowheight)))
              (insert (propertize " "
                                  'intangible 'editor
                                  'display shortcut-icon
                                  'pixel-occupied id
                                  'pixel-property :palette-shortcuts
                                  'pixel-update (lambda (colors)
                                                  (let ((icon (pixel-make-pixel (nth n colors) rowheight)))
                                                    (list 'display icon)))
                                  'pixel-toolbar t
                                  'keymap editor-keymap)))))))))

(defun pixel-editor-insert-palette (editor palette)
  (when (and editor palette)
    (save-excursion
      (let* ((id (pixel-editor-get editor :id))
             (ov (pixel-editor-get editor :ov-palette))
             (bg (pixel-editor-get editor :editor-background))
             (fg (pixel-editor-get editor :editor-foreground))
             (indentation (pixel-editor-get editor :editor-indentation))
             (rowlength (pixel-editor-get editor :palette-rowlength))
             (rowheight (pixel-editor-get editor :palette-rowheight))
             (start (overlay-start ov))
             (colors (plist-get palette :colors))
             (symbols (plist-get palette :symbols))
             (template (pixel-xpm-data (pixel-make-bitmap :width rowheight :height rowheight)))
             (whitespace (pixel-make-pixel bg indentation rowheight))
             (avg (pixel-palette-average palette))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t)
             (color-map (make-hash-table :test 'equal))
             (editor-keymap (pixel-make-editor-keymap editor (make-sparse-keymap)))
             (palette-keymap (pixel-make-palette-keymap editor editor-keymap))
             (line-end nil))
        (goto-char start)
        (when (<= (- (point) (point-at-bol)) 0)
          (insert (propertize " "
                              'intangible 'editor
                              'display whitespace
                              'pixel-occupied id
                              'pixel-palette t
                              'keymap editor-keymap)))
        (dotimes (n (length colors))
          (when (and rowlength (>= (- (point) (point-at-bol)) (+ (if indentation 1 0) rowlength)))
            (insert (propertize "\n"
                                'intangible 'editor
                                'pixel-occupied id
                                'pixel-palette t
                                'keymap editor-keymap))
            (insert (propertize " "
                                'intangible 'editor
                                'display whitespace
                                'pixel-occupied id
                                'pixel-palette t
                                'keymap editor-keymap)))
          (let* ((c (nth n colors))
                 (icon (pixel-make-pixel c rowheight))
                 (hover-face (pixel-make-hover-face "pixel-mode-palette-hover-face" n 0)))
            (puthash c (nth n symbols) color-map)
            (insert (propertize (if (string-equal c (car (last colors)))
                                    (propertize " " 'intangible 'editor)
                                  (propertize " "))
                                'display icon
                                'pixel-occupied id
                                'pixel-color c
                                'pixel-alpha 1
                                'pixel-palette-id n
                                'pixel-palette t
                                'mouse-face hover-face
                                'keymap palette-keymap))
            (when (and rowlength (>= (- (point) (point-at-bol)) (+ (if indentation 1 0) rowlength)))
              (add-text-properties (1- (point)) (point) (list 'intangible 'editor)))))
        (overlay-put ov 'pixel-color-map color-map)))))

(defun pixel-editor-insert-canvas (editor bitmap palette)
  (when (and editor bitmap palette)
    (save-excursion
      (let* ((id (pixel-editor-get editor :id))
             (ov (pixel-editor-get editor :ov-canvas))
             (bg (pixel-editor-get editor :editor-background))
             (zoomlevel (pixel-editor-get editor :editor-zoomlevel))
             (indentation (pixel-editor-get editor :editor-indentation))
             (w (plist-get bitmap :width))
             (h (plist-get bitmap :height))
             (start (overlay-start ov))
             (palette (pixel-find-palette :bitmap bitmap))
             (colors (apply 'vector (plist-get palette :colors)))
             (alphas (plist-get bitmap :alpha))
             (whitespace (pixel-make-pixel bg indentation (* zoomlevel 2)))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t)
             (editor-keymap (pixel-make-editor-keymap editor (make-sparse-keymap)))
             (canvas-keymap (pixel-make-canvas-keymap editor editor-keymap)))
        (delete-region (overlay-start ov) (overlay-end ov))
        (goto-char start)
        (dotimes (y h)
          (insert (propertize " "
                              'intangible 'editor
                              'display whitespace
                              'pixel-occupied id
                              'pixel-canvas t
                              'keymap editor-keymap))
          (dotimes (x w)
            (let* ((v (pixel-bitmap-ref bitmap x y))
                   (c (condition-case nil (elt colors v) (error "#000000")))
                   (a (condition-case nil (elt alphas v) (error 1)))
                   (pixel (pixel-make-pixel c (* zoomlevel 2)))
                   (hover-face (pixel-make-hover-face "pixel-mode-canvas-hover-face" x y)))
              (insert (propertize (if (eq x (- w 1))
                                      (propertize " " 'intangible 'editor)
                                    (propertize " "))
                                  'display pixel
                                  'pixel-occupied id
                                  'pixel-canvas t
                                  'pixel-color c
                                  'pixel-alpha a
                                  'pixel-x x
                                  'pixel-y y
                                  'line-height t
                                  'line-spacing nil
                                  'mouse-face hover-face
                                  'keymap canvas-keymap
                                  ))))
          (insert (propertize "\n"
                              'intangible 'editor
                              'pixel-occupied id
                              'pixel-canvas t
                              'line-height t
                              'line-spacing nil
                              'keymap editor-keymap))
          (move-overlay ov start (point)))))))

;; (setq pixel-editor-overlays
;;       '(:ov-complete :ov-source :ov-editor :ov-seperator1 :ov-tools :ov-seperator2 :ov-palette :ov-seperator3 :ov-canvas))

;; (setq pixel-editor-overlays
;;       '(:ov-complete :ov-source :ov-editor :ov-seperator2 :ov-palette :ov-seperator3 :ov-canvas))



(defvar pixel-editor-overlays
  '(:ov-complete :ov-source :ov-editor :ov-seperator2 :ov-palette :ov-seperator3 :ov-toolbar :ov-seperator4 :ov-canvas))

(defun pixel-editor-remove (editor)
  (let ((modified-state (buffer-modified-p)))
    (dolist (key pixel-editor-overlays)
      (when (eq key :ov-editor)
        (delete-region (overlay-start (plist-get editor key))
                       (overlay-end (plist-get editor key))))
      (when (plist-get editor key)
        (delete-overlay (plist-get editor key))))
    (unless (find :ov-source pixel-editor-overlays)
      (delete-overlay (plist-get editor :ov-source)))
    (delete-overlay (plist-get editor :ov-array))
    (pixel-editor-put editor nil)
    (set-buffer-modified-p modified-state)))

;; (pixel-editor-create (pixel-find-palette :id "bnw") (pixel-find-bitmap :id "test1") (pixel-find-bitmap :find-origin t :id "test1")
;;                      :background "#2f2f2f"
;;                      :foreground "#ffffff"
;;                      :source-background "#222222")

(defun pixel-editor-init (editor bitmap &optional palette)
  (unless palette
    (setq palette (pixel-find-palette :id (plist-get bitmap :palette-id))))

  (plist-put editor :id (plist-get bitmap :id))
  (unless (pixel-editor-get editor :palette-rowlength)
    (pixel-editor-put editor :palette-rowlength 32))
  (unless (pixel-editor-get editor :palette-rowheight)
    (pixel-editor-put editor :palette-rowheight 28))
  (unless (pixel-editor-get editor :toolbar-rowheight)
    (pixel-editor-put editor :toolbar-rowheight 28))

  (unless (string-equal (plist-get bitmap :palette-id) (pixel-editor-get editor :palette-id))
    (pixel-editor-put editor :palette-background (nth 0 (plist-get palette :colors)))
    (pixel-editor-put editor :palette-foreground (car (last (plist-get palette :colors))))
    (pixel-editor-put editor :palette-shortcuts (loop for c in (reverse (last (reverse (plist-get palette :colors)) 10))
                                                      collect c)))

  (pixel-editor-put editor :palette-id (plist-get bitmap :palette-id))

  (pixel-editor-put editor :bitmap-id (plist-get bitmap :id))
  (pixel-editor-put editor :bitmap-type (plist-get bitmap :type))
  (pixel-editor-put editor :bitmap-comma (plist-get bitmap :comma))
  (pixel-editor-put editor :bitmap-open (plist-get bitmap :open))
  (pixel-editor-put editor :bitmap-close (plist-get bitmap :close))
  (pixel-editor-put editor :bitmap-height (plist-get bitmap :height))
  (pixel-editor-put editor :bitmap-width (plist-get bitmap :width))
  (pixel-editor-put editor :bitmap-stride (plist-get bitmap :stride))
  (pixel-editor-put editor :bitmap-format (plist-get bitmap :format)))

(defun* pixel-editor-create (origin &key (background nil) (foreground nil) (source-background nil))
  (when (and origin)
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
                             (forward-line)
                             (point-at-bol)))
             (array-beg (plist-get origin :array-beginning))
             (array-beg-offset (- array-beg (plist-get origin :beginning)))
             (array-end (plist-get origin :array-end))
             (array-end-offset (- array-end (plist-get origin :beginning)))
             (ov-array (unless (find :ov-source pixel-editor-overlays)
                         (let ((ov (make-overlay array-beg array-end)))
                           (overlay-put ov 'invisible t)
                           ov)))
             ;; when :ov-source is not in pixel-editor-overlays, we create a overlay here and hide the text
             (ov-source (unless (find :ov-source pixel-editor-overlays)
                          (let ((ov (make-overlay first-pos src-end)))
                            (overlay-put ov 'invisible t)
                            ov)))
             ;; need overlay symbol so we can add the editor as property to it
             (ov-complete nil)
             (ov-editor nil)
             (src-text (unless (overlayp ov-source)
                         (let ((str (buffer-substring first-pos src-end)))
                           (delete-region first-pos src-end)
                           str)))
             (editor (list :ov-source ov-source
                           :ov-array ov-array))
             (last-pos (if (overlayp ov-source) src-end first-pos))
             (next-pos last-pos)
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t)
             (id (plist-get origin :id)))
        (plist-put editor :id id)
        (pixel-editor-put editor :editor-foreground foreground)
        (pixel-editor-put editor :editor-background background)
        (pixel-editor-put editor :editor-zoomlevel 8)
        (pixel-editor-put editor :editor-indentation 20)
        (dolist (key pixel-editor-overlays)
          (cond ((eq :ov-complete key)
                 (setq ov-complete (make-overlay first-pos (+ last-pos 1)))
                 (setq editor (plist-put editor :ov-complete ov-complete)))
                ((eq :ov-editor key)
                 (setq next-pos (progn (goto-char last-pos)
                                       (insert (propertize "\n"
                                                           'intangible 'editor
                                                           'pixel-overlay key
                                                           ;;'line-height t
                                                           ;;'line-spacing nil
                                                           ))
                                       (point-at-bol)))
                 (setq ov-editor (make-overlay last-pos next-pos))
                 (overlay-put ov-editor 'face `((:background ,background)
                                                (:foreground ,foreground)))
                 (setq editor (plist-put editor :ov-editor ov-editor)))
                ((eq :ov-source key)
                 (setq next-pos (progn (goto-char last-pos)
                                       (insert src-text)
                                       (point-at-bol)))
                 (setq ov-source (make-overlay last-pos next-pos))
                 (setq ov-array (make-overlay (+ last-pos array-beg-offset) (+ last-pos array-end-offset)))
                 (overlay-put ov-source 'face `((:background ,source-background)
                                                (:foreground ,foreground)))
                 (overlay-put ov-array 'face `((:background ,source-background)
                                               (:foreground ,foreground)))
                 (setq editor (plist-put editor :ov-source ov-source))
                 (setq editor (plist-put editor :ov-array ov-array))
                 (setq last-pos next-pos))
                (t
                 (setq next-pos (progn (goto-char last-pos)
                                       (insert (propertize "\n"
                                                           'intangible 'editor
                                                           'pixel-overlay key
                                                           ;;'line-height t
                                                           ;;'line-spacing nil
                                                           ))
                                       (point-at-bol)))
                 (let ((ov (make-overlay last-pos next-pos)))
                   (overlay-put ov 'face `((:background ,background)
                                           (:foreground ,foreground)))
                   (setq editor (plist-put editor key ov))
                   (setq last-pos next-pos)))))
        (overlay-put ov-complete 'pixel-editor editor)))))

;;
;; Source
;;

(defun pixel-source-color (editor color &optional alpha)
  (let ((format (pixel-editor-get editor :bitmap-format)))
    (cond ((string-equal format "palette")
           (let ((ov-palette (pixel-editor-get editor :ov-palette)))
             (gethash color (overlay-get ov-palette 'pixel-color-map))))
          ((or (string-equal format "rgb")
               (string-equal format "rgba"))
           (let* ((comma (pixel-editor-get editor :bitmap-comma))
                  (type (pixel-editor-get editor :bitmap-type)))
             (mapconcat (lambda (c)
                          (prin1-to-string (cond ((string-equal type "float")
                                                  c)
                                                 ((string-equal type "int")
                                                  (truncate (* c 255))))))
                        (nconc (color-name-to-rgb color) (when (and alpha (string-equal format "rgba")) (list alpha)))
                        comma))))))

(defun pixel-source-replace-pixel (editor x y color)
  (let* ((width (pixel-editor-get editor :bitmap-width))
         (height (pixel-editor-get editor :bitmap-height))
         (stride (pixel-editor-get editor :bitmap-stride))
         (comma (pixel-editor-get editor :bitmap-comma))
         (xs (if (listp x) x (list x)))
         (ys (if (listp y) y (list y)))
         (colors (if (listp color) color (make-list (length xs) color)))
         (pixels (sort (mapcar* #'list xs ys colors)
                       (lambda (a b)
                         (if (eq (nth 1 a) (nth 1 b))
                             (< (nth 0 a) (nth 0 b))
                           (< (nth 1 a) (nth 1 b))))))
         (ov-array (pixel-editor-get editor :ov-array))
         (lastx 0)
         (lasty 0))
    (with-current-buffer (overlay-buffer ov-array)
      (save-excursion
        (goto-char (overlay-start ov-array))
        (dolist (p pixels)
          (let ((x (nth 0 p))
                (y (nth 1 p))
                (color (nth 2 p)))
            (when (> (- y lasty) 0)
              (goto-char (point-at-bol))
              (setq lasty (dotimes (yi (- y lasty) (+ lasty yi))
                            (re-search-forward (concat "\\(^[^0-9.]*\\)?\\(?:[0-9.]+[" (regexp-quote comma) "\n]*\\)\\{" (prin1-to-string (* width stride)) "\\}") (overlay-end ov-array)))
                    lastx 0))
            (setq lastx (dotimes (xi (- x lastx) (+ lastx xi))
                          (re-search-forward (concat "\\(?:[0-9.]+[" (regexp-quote comma) "\n]*\\)\\{"(prin1-to-string stride) "\\}")  (overlay-end ov-array))))
            (save-excursion
              (re-search-forward (concat "\\(\\(?:[0-9.]+\\([" (regexp-quote comma) "\n]*\\)\\)\\{" (prin1-to-string stride) "\\}\\)") (overlay-end ov-array))
              ;;(print (match-string-no-properties 1))
              (replace-match (concat color (match-string 2)) t nil nil 1))))))))

;;
;; Canvas Tools
;;

(defun pixel-neighbours (x y)
  (mapcar (lambda (shift) (list (+ x (nth 0 shift))
                                (+ y (nth 1 shift))))
          '((0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1))))

(defun pixel-tool-fill (input type editor x0 y0 color0 alpha0 &optional x1 y1 color1 alpha1 state)
  (unless (and x1 y1)
    (let* ((start-x x0)
           (start-y y0)
           (compare-color color0)
           (fill-alpha alpha0)
           (fill-color (pixel-editor-get editor :palette-foreground))
           (w (pixel-editor-get editor :bitmap-width))
           (h (pixel-editor-get editor :bitmap-height))
           (neighbour-map (make-vector (* w h) nil))
           (neighbour-queue (pixel-neighbours start-x start-y))
           (center nil)
           (updates nil))
      (push (list start-x start-y fill-color fill-alpha) updates)
      (setf (elt neighbour-map (pixel-bitmap-index w start-x start-y)) t)
      (while (setq center (pop neighbour-queue))
        (let ((center-x (nth 0 center))
              (center-y (nth 1 center)))
          (when (and (>= center-x 0)
                     (< center-x w)
                     (>= center-y 0)
                     (< center-y h))
            ;; if n has same color as compare-color
            ;;   make update to fill with fill-color
            ;;   check neighbours not marked in neighbour-map
            ;;   put not marked neighbours of n in neighbour-queue
            (when (eq (pixel-canvas-color editor center-x center-y) compare-color)
              (push (list center-x center-y fill-color fill-alpha) updates)
              (dolist (neighbour (pixel-neighbours center-x center-y))
                (let ((neighbour-x (nth 0 neighbour))
                      (neighbour-y (nth 1 neighbour)))
                  (when (and (>= neighbour-x 0)
                             (< neighbour-x w)
                             (>= neighbour-y 0)
                             (< neighbour-y h)
                             (not (elt neighbour-map (pixel-bitmap-index w neighbour-x neighbour-y))))
                    (push neighbour neighbour-queue)))))
            ;; mark n in neighbour-map as processed
            (setf (elt neighbour-map (pixel-bitmap-index w center-x center-y)) t))))
      (plist-put state :updates updates))))

(defun pixel-tool-draw (input type editor x0 y0 color0 alpha0 &optional x1 y1 color1 alpha1 state)
  (let ((x (or x1 x0))
        (y (or y1 y0))
        (color (or color1 color0))
        (alpha (or alpha1 alpha0)))
    (plist-put state :updates
               (nconc (list (list x y
                                  (cond ((or (eq input 'mouse1) (eq input 'keyboard))
                                         (pixel-editor-get editor :palette-foreground))
                                        ((eq input 'mouse3)
                                         (pixel-editor-get editor :palette-background)))
                                  alpha))
                      (plist-get state :updates)))))

(defun pixel-tool-rectangle (input type editor x0 y0 color0 alpha0 &optional x1 y1 color1 alpha1 state)
  (let* ((w (pixel-editor-get editor :bitmap-width))
         (h (pixel-editor-get editor :bitmap-height))
         (color (pixel-editor-get editor :palette-foreground))
         (alpha 1)
         (fill (plist-get state :fill)))
    (when (and x0 x1 y0 y1)
      (let ((updates nil)
            (old-layer (or (plist-get state :layer)
                           (make-vector (* w h) nil)))
            (new-layer (make-vector (* w h) nil))
            (old-layer-overlays (plist-get state :layer-overlays))
            (new-layer-overlays nil)
            (rect-w (- x1 x0))
            (rect-h (- y1 y0)))
        (dotimes (rect-x (1+ (abs rect-w)))
          (dotimes (rect-y (1+ (abs rect-h)))
            (let* ((x (+ (if (< x0 x1) x0 x1) rect-x))
                   (y (+ (if (< y0 y1) y0 y1) rect-y))
                   (i (pixel-bitmap-index w x y))
                   (ov (elt old-layer i)))
              (when (or fill
                        (eq x x0)
                        (eq x x1)
                        (eq y y0)
                        (eq y y1))
                (when (overlayp ov)
                  (setf (elt new-layer i) ov)
                  (push ov new-layer-overlays))
                (push (list x y color alpha) updates)))))
        (dolist (ov old-layer-overlays)
          (let ((x (overlay-get ov 'pixel-x))
                (y (overlay-get ov 'pixel-y)))
            (unless (overlayp (elt new-layer (pixel-bitmap-index w x y)))
              (delete-overlay ov))))
        (plist-put (plist-put (plist-put state
                                         :updates updates)
                              :layer new-layer)
                   :layer-overlays new-layer-overlays)))))

(defun pixel-tool-rectangle-filled (input type editor x0 y0 color0 alpha0 &optional x1 y1 color1 alpha1 state)
  (pixel-tool-rectangle input type editor x0 y0 color0 alpha0 x1 y1 color1 alpha1 (plist-put state :fill t)))

(defun pixel-canvas-point (editor &optional x y)
  (unless (and x y)
    (setq x (get-text-property (point) 'pixel-x)
          y (get-text-property (point) 'pixel-y)))
  (when (and x y)
    (let ((w (pixel-editor-get editor :bitmap-width))
          (h (pixel-editor-get editor :bitmap-height))
          (ov (pixel-editor-get editor :ov-canvas)))
      (+ (overlay-start ov) 1 (* y 2) (* y w) x))))

(defun pixel-canvas-color (editor &optional x y)
  (unless (and x y)
    (setq x (get-text-property (point) 'pixel-x)
          y (get-text-property (point) 'pixel-y)))
  (when (and x y)
    (get-text-property (pixel-canvas-point editor x y) 'pixel-color)))

(defun pixel-canvas-alpha (editor &optional x y)
  (unless (and x y)
    (setq x (get-text-property (point) 'pixel-x)
          y (get-text-property (point) 'pixel-y)))
  (when (and x y)
    (get-text-property (pixel-canvas-point editor x y) 'pixel-alpha)))

(defun pixel-canvas-merge (editor state)
  (let ((layer-overlays (plist-get state :layer-overlays)))
    (dolist (ov layer-overlays)
      (delete-overlay ov)))
  (dolist (u (plist-get state :updates))
    (let* ((x (nth 0 u))
           (y (nth 1 u))
           (color (nth 2 u))
           (alpha (nth 3 u))
           (zoomlevel (pixel-editor-get editor :editor-zoomlevel))
           (beg (pixel-canvas-point editor x y))
           (end (1+ (pixel-canvas-point editor x y))))
      (put-text-property beg end 'display (pixel-make-pixel color (* zoomlevel 2)))
      (put-text-property beg end 'pixel-color color)
      (put-text-property beg end 'pixel-alpha alpha)
      (pixel-source-replace-pixel editor x y (pixel-source-color editor color alpha)))))

(defun* pixel-canvas-layer (editor state)
  (let* ((w (pixel-editor-get editor :bitmap-width))
         (h (pixel-editor-get editor :bitmap-height))
         (updates (sort (copy-list (plist-get state :updates))
                        (lambda (a b)
                          (if (eq (nth 1 a) (nth 1 b))
                              (< (nth 0 a) (nth 0 b))
                            (< (nth 1 a) (nth 1 b))))))
         (layer (or (plist-get state :layer)
                    (make-vector (* w h) nil)))
         (layer-overlays (plist-get state :layer-overlays))
         (zoomlevel (pixel-editor-get editor :editor-zoomlevel)))
    (dotimes (i (length updates))
      (let* ((u (nth i updates))
             (x (nth 0 u))
             (y (nth 1 u))
             (layer-index (pixel-bitmap-index w x y)))
        (unless (elt layer layer-index)
          (let* ((color (nth 2 u))
                 (alpha (nth 3 u))
                 (beg (pixel-canvas-point editor x y))
                 (end (1+ beg))
                 (ov (make-overlay beg end)))
            (overlay-put ov 'display (pixel-make-pixel color (* zoomlevel 2)))
            (overlay-put ov 'pixel-x x)
            (overlay-put ov 'pixel-y y)
            (overlay-put ov 'pixel-color color)
            (overlay-put ov 'pixel-alpha alpha)
            (setf (elt layer layer-index) ov)
            (push ov layer-overlays)))))
    (plist-put (plist-put state :layer layer) :layer-overlays layer-overlays)))

(defun pixel-canvas-refresh (editor)
  (let* ((modified-state (buffer-modified-p))
         (ov-canvas (pixel-editor-get editor :ov-canvas))
         (bitmap-id (pixel-editor-get editor :bitmap-id))
         (palette-id (pixel-editor-get editor :palette-id))
         (bitmap (pixel-find-bitmap :id bitmap-id))
         (palette (pixel-find-palette :id palette-id))
         (p (point)))
    (delete-region (overlay-start ov-canvas) (overlay-end ov-canvas))
    (pixel-editor-init editor bitmap palette)
    (pixel-editor-insert-canvas editor bitmap palette)
    (set-buffer-modified-p modified-state)
    (goto-char p)))

(defun pixel-canvas-replace (editor bitmap)
  (with-current-buffer (pixel-editor-buffer editor)
    (save-excursion
      (let* ((ov-array (pixel-editor-get editor :ov-array))
             (ov-source (pixel-editor-get editor :ov-source))
             (id (pixel-editor-get editor :bitmap-id))
             (new-palette (plist-get bitmap :palette-id))
             (new-format (plist-get bitmap :format))
             (new-type (plist-get bitmap :type))
             (new-width (plist-get bitmap :width))
             (new-height (plist-get bitmap :height))
             (new-stride (plist-get bitmap :stride))
             (new-array (plist-get bitmap :array))
             (comma (plist-get bitmap :comma)))
        (goto-char (overlay-start ov-source))
        (looking-at (pixel-regex :bitmap id :id id))
        (when (and new-palette
                   (match-string 2)
                   (and (not (save-match-data (string-match (replace-regexp-in-string "[ \t]+" "[-_]" new-palette) (match-string 2))))))
          (replace-match new-palette t nil nil 2))
        (when (and new-format
                   (match-string 3))
          (replace-match new-format t nil nil 3))
        (when (and new-type
                   (match-string 4))
          (replace-match new-type t nil nil 4))
        (when (and new-width
                   (match-string 5))
          (replace-match (prin1-to-string new-width) t nil nil 5))
        (when (and new-height
                   (match-string 6))
          (replace-match (prin1-to-string new-height) t nil nil 6))
        (when (and new-stride
                   (match-string 7))
          (replace-match (prin1-to-string new-stride) t nil nil 7))
        (when (and new-array
                   (match-string 9))
          (let* ((w 0)
                 (h 0)
                 (default (cond ((string-equal new-format "palette")
                                 0)
                                ((string-equal new-format "rgb")
                                 (list 0.0 0.0 0.0))))
                 (palette (when (not (string-equal new-format "palette"))
                            (save-match-data
                              (pixel-find-palette :id new-palette))))
                 (new-array-string (mapconcat #'identity
                                              (loop for i from 0 below (* new-width new-height)
                                                    do (if (< w new-width)
                                                           (setq w (1+ w))
                                                         (setq w 1)
                                                         (setq h (1+ h)))
                                                    collect (let ((color (cond ((string-equal new-format "palette")
                                                                                (prin1-to-string (condition-case nil (elt new-array i) (error default))))
                                                                               ((string-equal new-format "rgb")
                                                                                (mapconcat #'identity
                                                                                           (mapcar #'prin1-to-string
                                                                                                   (pixel-color-name-to-rgb new-type (condition-case nil (elt (plist-get palette :colors) (elt new-array i)) (error default))))
                                                                                           comma)))))
                                                              (concat color
                                                                      (unless (eq i (1- (* new-width new-height))) (replace-regexp-in-string "[ \t]*" "" comma))
                                                                      (when (and (eq w new-width) (< h (1- new-height))) "\n"))))
                                              " ")))
            (replace-match (concat  new-array-string "\n") t nil nil 9)))
        (pixel-canvas-refresh editor)
        ))))

(defun pixel-canvas-resize (editor add-width add-height)
  (interactive
   (let ((editor (pixel-find-editor :point (point))))
     (when editor
       (let* ((w (pixel-editor-get editor :bitmap-width))
              (h (pixel-editor-get editor :bitmap-height))
              (size (read-string "New size: " (concat (prin1-to-string w) "x" (prin1-to-string h))))
              (tokens (append (split-string size "x") (list w h)))
              (new-w (condition-case nil (read (nth 0 tokens)) (error w)))
              (new-h (condition-case nil (read (nth 1 tokens)) (error h))))
         (list editor
               (- new-w w)
               (- new-h h))))))
  (let* ((id (pixel-editor-get editor :bitmap-id))
         (bitmap (pixel-bitmap-resize (pixel-find-bitmap :id id) add-width add-height))
         (l (line-number-at-pos))
         (p (point)))
    (pixel-canvas-replace editor bitmap)
    (goto-line (+ l add-height))
    (pixel-canvas-view editor p)))

(defun pixel-canvas-view (editor &optional p)
  (let* ((ov-editor (pixel-editor-get editor :ov-editor))
         (s (overlay-start ov-editor)))
    (save-excursion
      (goto-char (or p s))
      (while (and (> (window-start) s)
                  (not (eq (point) (point-min))))
        (forward-line -1)
        (recenter)))
    (save-excursion
      (goto-char (or p s))
      (while (and (< (window-start) s)
                  (not (eq (point) (point-max))))
        (forward-line 1)
        (recenter)))))

;;
;; Action Utilities
;;

(defun pixel-editor-tool-switch (editor tool)
  (message "%s" (prin1-to-string tool))
  (pixel-editor-put editor :tool-current tool))

(defun pixel-make-canvas-keypress (input type editor &optional tool)
  (lambda (&optional pos)
    (interactive)
    (let* ((x (get-text-property (point) 'pixel-x))
           (y (get-text-property (point) 'pixel-y))
           (color (get-text-property (point) 'pixel-color))
           (alpha (get-text-property (point) 'pixel-alpha))
           (tool (or tool (pixel-editor-get editor :tool-current))))
      (cond ((or (eq tool 'pixel-tool-rectangle)
                 (eq tool 'pixel-tool-rectangle-filled))
             (let ((event nil)
                   (state (funcall tool input type editor x y color alpha))
                   (last-x x)
                   (last-y y)
                   (done t)
                   (inhibit-quit t))
               (setq done (with-local-quit
                            (while (progn
                                     (setq event (read-event))
                                     (not (or (eq event 'escape)
                                              (eq event 32)
                                              (eq event 'return))))
                              (cond ((eq event 'up)
                                     (previous-line 1))
                                    ((eq event 'right)
                                     (right-char 1))
                                    ((eq event 'down)
                                     (next-line 1))
                                    ((eq event 'left)
                                     (left-char 1)))
                              (let ((drag-x (get-text-property (point) 'pixel-x))
                                    (drag-y (get-text-property (point) 'pixel-y))
                                    (drag-color (get-text-property (point) 'pixel-color))
                                    (drag-alpha (get-text-property (point) 'pixel-alpha)))
                                (unless (or (and (eq last-x drag-x) (eq last-y drag-y))
                                            (not (and drag-x drag-y drag-color drag-alpha)))
                                  (setq state (condition-case nil
                                                  (pixel-canvas-layer editor
                                                                      (funcall tool input type editor
                                                                               x y color alpha
                                                                               drag-x drag-y drag-color drag-alpha
                                                                               state))
                                                (error (dolist (ov (plist-get state :layer-overlays))
                                                         (delete-overlay ov))))
                                        last-x drag-x
                                        last-y drag-y))))
                            t))
               (if (or (not done)
                       (eq event 'escape))
                   (dolist (ov (plist-get state :layer-overlays))
                     (delete-overlay ov))
                 (pixel-canvas-merge editor state))))
            (t
             (pixel-canvas-merge editor (funcall tool input type editor x y color alpha)))))))

(defun pixel-make-canvas-motion (input type editor &optional tool)
  (lambda (event)
    (interactive "e")
    (let* ((tool (or tool (pixel-editor-get editor :tool-current)))
           (p (posn-point (event-start event)))
           (x (get-text-property p 'pixel-x))
           (y (get-text-property p 'pixel-y))
           (color (get-text-property p 'pixel-color))
           (alpha (get-text-property p 'pixel-alpha))
           (state (funcall tool input type editor x y color alpha))
           (last-x x)
           (last-y y)
           (layer nil)
           (done t))
      (track-mouse
        (setq done (with-local-quit
                     (while (progn
                              (setq event (read-event))
                              (or (mouse-movement-p event)
                                  (memq (car-safe event) '(switch-frame select-window))))
                       (mouse-set-point event)
                       (let ((drag-x (get-text-property (point) 'pixel-x))
                             (drag-y (get-text-property (point) 'pixel-y))
                             (drag-color (get-text-property (point) 'pixel-color))
                             (drag-alpha (get-text-property (point) 'pixel-alpha)))
                         (unless (or (and (eq last-x drag-x) (eq last-y drag-y))
                                     (not (and drag-x drag-y drag-color drag-alpha)))
                           (setq state (condition-case nil
                                           (pixel-canvas-layer editor
                                                               (funcall tool input type editor
                                                                        x y color alpha
                                                                        drag-x drag-y drag-color drag-alpha
                                                                        state))
                                         (error (dolist (ov (plist-get state :layer-overlays))
                                                  (delete-overlay ov))))
                                 last-x drag-x
                                 last-y drag-y))))
                     t)))
      (if (or (not done)
              (eq event 'escape))
          (dolist (ov (plist-get state :layer-overlays))
            (delete-overlay ov))
        (pixel-canvas-merge editor state)))))

;; (let ((event nil))
;;   (while (not (eq event 'escape))
;;     (setq event (read-event))
;;     (print event)))

(defun pixel-editor-shortcut-get (editor n)
  (let ((shortcuts (pixel-editor-get editor :palette-shortcuts)))
    (pixel-editor-foreground-set editor (nth n shortcuts))))

(defun pixel-editor-shortcut-set (editor n &optional color)
  (let ((shortcuts (pixel-editor-get editor :palette-shortcuts))
        (color (or color (get-text-property (point) 'pixel-color))))
    (setf (nth n shortcuts) color)
    (pixel-editor-update editor :palette-shortcuts shortcuts)
    (pixel-editor-put editor :palette-shortcuts shortcuts)))

(defun pixel-editor-foreground-set (editor &optional color)
  (let ((color (or color (get-text-property (point) 'pixel-color))))
    (pixel-editor-update editor :palette-foreground color)
    (pixel-editor-put editor :palette-foreground color)))

(defun pixel-canvas-pick-color (editor)
  (let ((color (pixel-canvas-color editor)))
    (when color
      (pixel-editor-update editor :palette-foreground color)
      (pixel-editor-put editor :palette-foreground color))))

(defun* pixel-make-action (editor action &rest args)
  (lambda (&optional pos)
    (interactive)
    (apply action (append (list editor) args))))

(defun* pixel-make-mouse-action (editor action &rest args)
  (lambda (event)
    (interactive "e")
    (mouse-set-point event)
    (apply action (append (list editor) args))))


(defun pixel-make-editor-keymap (editor map)
  (suppress-keymap map)
  (define-key map (kbd "d") (pixel-make-action editor 'pixel-editor-tool-switch 'pixel-tool-draw))
  (define-key map (kbd "f") (pixel-make-action editor 'pixel-editor-tool-switch 'pixel-tool-fill))
  (define-key map (kbd "r") (pixel-make-action editor 'pixel-editor-tool-switch 'pixel-tool-rectangle))
  (define-key map (kbd "R") (pixel-make-action editor 'pixel-editor-tool-switch 'pixel-tool-rectangle-filled))
  (define-key map (kbd "p") (pixel-make-action editor 'pixel-canvas-pick-color))
  (define-key map (kbd "<tab>") (pixel-make-action editor 'pixel-canvas-view))
  map)

(defun pixel-make-canvas-keymap (editor map)
  (suppress-keymap map)
  (define-key map (kbd "<RET>") (pixel-make-canvas-keypress 'keyboard 'single editor))
  (define-key map (kbd "<SPC>") (pixel-make-canvas-keypress 'keyboard 'single editor))
  (define-key map (kbd "<double-mouse-1>") (pixel-make-canvas-motion 'mouse1 'double editor))
  (define-key map (kbd "<double-mouse-2>") (pixel-make-canvas-motion 'mouse2 'double editor))
  (define-key map (kbd "<double-mouse-3>") (pixel-make-canvas-motion 'mouse3 'double editor))
  (define-key map (kbd "<down-mouse-1>") (pixel-make-canvas-motion 'mouse1 'single editor))
  (define-key map (kbd "<down-mouse-2>") (pixel-make-canvas-motion 'mouse2 'single editor))
  (define-key map (kbd "<down-mouse-3>") (pixel-make-canvas-motion 'mouse3 'single editor))
  (define-key map (kbd "C-<right>") (pixel-make-action editor 'pixel-canvas-resize 1 0))
  (define-key map (kbd "C-<left>") (pixel-make-action editor 'pixel-canvas-resize -1 0))
  (define-key map (kbd "C-<down>") (pixel-make-action editor 'pixel-canvas-resize 0 1))
  (define-key map (kbd "C-<up>") (pixel-make-action editor 'pixel-canvas-resize 0 -1))
  (define-key map (kbd "1") (pixel-make-action editor 'pixel-editor-shortcut-get 0))
  (define-key map (kbd "2") (pixel-make-action editor 'pixel-editor-shortcut-get 1))
  (define-key map (kbd "3") (pixel-make-action editor 'pixel-editor-shortcut-get 2))
  (define-key map (kbd "4") (pixel-make-action editor 'pixel-editor-shortcut-get 3))
  (define-key map (kbd "5") (pixel-make-action editor 'pixel-editor-shortcut-get 4))
  (define-key map (kbd "6") (pixel-make-action editor 'pixel-editor-shortcut-get 5))
  (define-key map (kbd "7") (pixel-make-action editor 'pixel-editor-shortcut-get 6))
  (define-key map (kbd "8") (pixel-make-action editor 'pixel-editor-shortcut-get 7))
  (define-key map (kbd "9") (pixel-make-action editor 'pixel-editor-shortcut-get 8))
  (define-key map (kbd "0") (pixel-make-action editor 'pixel-editor-shortcut-get 9))
  map)

(defun pixel-make-palette-keymap (editor map)
  (suppress-keymap map)
  (define-key map (kbd "<RET>") (pixel-make-action editor 'pixel-editor-foreground-set))
  (define-key map (kbd "<SPC>") (pixel-make-action editor 'pixel-editor-foreground-set))
  (define-key map (kbd "<down-mouse-1>") (pixel-make-mouse-action editor 'pixel-editor-foreground-set))
  (define-key map (kbd "<down-mouse-2>") (pixel-make-mouse-action editor 'pixel-editor-foreground-set))
  (define-key map (kbd "<down-mouse-3>") (pixel-make-mouse-action editor 'pixel-editor-foreground-set))
  (define-key map (kbd "1") (pixel-make-action editor 'pixel-editor-shortcut-set 0))
  (define-key map (kbd "2") (pixel-make-action editor 'pixel-editor-shortcut-set 1))
  (define-key map (kbd "3") (pixel-make-action editor 'pixel-editor-shortcut-set 2))
  (define-key map (kbd "4") (pixel-make-action editor 'pixel-editor-shortcut-set 3))
  (define-key map (kbd "5") (pixel-make-action editor 'pixel-editor-shortcut-set 4))
  (define-key map (kbd "6") (pixel-make-action editor 'pixel-editor-shortcut-set 5))
  (define-key map (kbd "7") (pixel-make-action editor 'pixel-editor-shortcut-set 6))
  (define-key map (kbd "8") (pixel-make-action editor 'pixel-editor-shortcut-set 7))
  (define-key map (kbd "9") (pixel-make-action editor 'pixel-editor-shortcut-set 8))
  (define-key map (kbd "0") (pixel-make-action editor 'pixel-editor-shortcut-set 9))
  map)

(provide 'pixel-editor)
