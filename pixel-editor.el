;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(require 'pixel-bitmap nil 'noerror)

(defun pixel-editor-insert-toolbar (editor &rest tools)
  (pixel-editor-put editor :tool-current 'draw))

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
             (p (overlay-start ov))
             (colors (plist-get palette :colors))
             (symbols (plist-get palette :symbols))
             (template (pixel-xpm-data (pixel-make-bitmap :width rowheight :height rowheight :background 0)))
             (whitespace (pixel-make-pixel bg indentation rowheight))
             (avg (pixel-palette-average palette))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t)
             (id (plist-get editor :id))
             (color-map (make-hash-table :test 'equal)))
        (goto-char p)
        (when (<= (- (point) (point-at-bol)) 0)
          (insert (propertize " "
                              'intangible 'editor
                              'display whitespace
                              'pixel-occupied id
                              'palette t)))
        (dotimes (n (length colors))
          (when (and rowlength (>= (- (point) (point-at-bol)) (+ (if indentation 1 0) rowlength)))
            (insert (propertize "\n"
                                'intangible 'editor
                                'pixel-occupied id
                                'pixel-palette t
                                ;;'line-height t
                                ;;'line-spacing nil
                                ))
            (insert (propertize " "
                                'intangible 'editor
                                'display whitespace
                                'pixel-occupied id
                                'pixel-palette t)))
          (let* ((c (nth n colors))
                 (icon (pixel-make-pixel c rowheight))
                 (hover-face (pixel-make-hover-face "pixel-mode-palette-hover-face" (color-complement-hex avg) c)))
            (puthash c (nth n symbols) color-map)
            (insert (propertize (if (string-equal c (car (last colors)))
                                    (propertize " " 'intangible 'editor) ;; 
                                  (propertize " "))
                                'display icon
                                'pixel-occupied id
                                'pixel-color c
                                'pixel-color-id n
                                'pixel-palette t
                                'mouse-face hover-face
                                'keymap (pixel-editor-palette-keymap editor c)))))
        (overlay-put ov 'pixel-color-map color-map)))))

(defun pixel-editor-insert-canvas (editor palette bitmap)
  (when (and editor palette bitmap)
    (save-excursion
      (let* ((id (pixel-editor-get editor :id))
             (ov (pixel-editor-get editor :ov-canvas))
             (bg (pixel-editor-get editor :editor-background))
             (zoomlevel (pixel-editor-get editor :editor-zoomlevel))
             (indentation (pixel-editor-get editor :editor-indentation))
             (w (plist-get bitmap :width))
             (h (plist-get bitmap :height))
             (p (overlay-start ov))
             (colors (apply 'vector (plist-get palette :colors)))
             (avg (pixel-palette-average palette))
             (whitespace (pixel-make-pixel bg indentation (* zoomlevel 2)))
             (id (plist-get bitmap :id))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t))
        (goto-char p)
        (dotimes (y h)
          (insert (propertize " "
                              'intangible 'editor
                              'display whitespace
                              'pixel-occupied id
                              'pixel-canvas t))
          (dotimes (x w)
            (let* ((v (pixel-bitmap-ref bitmap x y))
                   (c (elt colors v))
                   (pixel (pixel-make-pixel c (* zoomlevel 2)))
                   (hover-face (pixel-make-hover-face "pixel-mode-canvas-hover-face" (color-complement-hex avg) nil x y)))
              (insert (propertize (if (eq x (- w 1))
                                      (propertize " " 'intangible 'editor) 
                                    (propertize " "))
                                  'display pixel
                                  'pixel-occupied id
                                  'pixel-canvas t
                                  'pixel-color c
                                  'pixel-x x
                                  'pixel-y y
                                  'line-height t
                                  'line-spacing nil
                                  'mouse-face hover-face
                                  'keymap (pixel-editor-canvas-keymap editor x y)))))
          (insert (propertize "\n"
                              'intangible 'editor
                              'pixel-occupied id
                              'pixel-canvas t
                              'line-height t
                              'line-spacing nil
                              )))))))

(defun pixel-make-image (&rest keys)
  (find-image (list keys)))

(defun pixel-make-hover-face (id fg &optional hex x y)
  (let* ((sym (intern (cond ((and x y)
                             (format "%s|%s|%d|%d" id (replace-regexp-in-string "#" "" fg) x y))
                            ((stringp hex)
                             (format "%s|%s|%s" id (replace-regexp-in-string "#" "" fg) (replace-regexp-in-string "#" "" hex)))
                            (t
                             (format "%s|%s" id (replace-regexp-in-string "#" "" fg))))))
         (face (make-face sym)))
    (face-spec-reset-face face)
    (set-face-attribute face nil :box (list :line-width 3 :color fg :style nil))
    face))

(defun pixel-copy-hover-face (face &optional hex x y)
  (print (face-attribute face :box t 'default))
  (let* ((sym (intern (cond ((and x y)
                             (replace-regexp-in-string "^.*\\(|[0-9]+|[0-9]+\\)$" (concat "|" (prin1-to-string x) "|" (prin1-to-string y)) (prin1-to-string face) nil nil 1))
                            ((stringp hex)
                             (replace-regexp-in-string "^.*\\(|[0-9a-fA-F]+\\)$" (concat "|" (replace-regexp-in-string "#" "" hex)) (prin1-to-string face) nil nil 1)))))
         (new-face (make-face sym)))
    (set-face-attribute new-face nil :box (print (face-attribute face :box t 'default)))
    new-face))

(setq pixel-pixel-cache (make-hash-table :test 'equal))

(defun pixel-make-pixel (color width &optional height)
  (let ((key (format "xpm|%d*%d|%s" width (or height width) (replace-regexp-in-string "#" "" color))))
    (cons 'image (cdr (or (gethash key pixel-pixel-cache nil)
                          (puthash key (let ((template (pixel-xpm-data (pixel-make-bitmap :width width :height (or height width) :background 0))))
                                         (pixel-make-image :type 'xpm
                                                           :data template
                                                           :color-symbols `(("col0"  . ,color))
                                                           :height (or height width)))
                                   pixel-pixel-cache))))))

;; (setq pixel-editor-overlays
;;       '(:ov-complete :ov-source :ov-editor :ov-seperator1 :ov-tools :ov-seperator2 :ov-palette :ov-seperator3 :ov-canvas))

;; (setq pixel-editor-overlays
;;       '(:ov-complete :ov-source :ov-editor :ov-seperator2 :ov-palette :ov-seperator3 :ov-canvas))

(defvar pixel-editor-overlays
  '(:ov-complete :ov-source :ov-editor :ov-seperator2 :ov-palette :ov-seperator3 :ov-canvas))

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

(defun* pixel-editor-create (bitmap palette origin &key (background nil) (foreground nil) (source-background nil))
  (when (and bitmap origin)
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
             (id (plist-get bitmap :id))
             (palette-id (plist-get bitmap :palette-id))
             (editor (list :id id
                           :palette-id palette-id
                           :ov-source ov-source
                           :ov-array ov-array))
             (last-pos (if (overlayp ov-source) src-end first-pos))
             (next-pos last-pos)
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t))
        (pixel-editor-put editor :editor-foreground foreground)
        (pixel-editor-put editor :editor-background background)
        (pixel-editor-put editor :editor-zoomlevel 12)
        (pixel-editor-put editor :editor-indentation 20)
        (pixel-editor-put editor :palette-id (plist-get bitmap :palette-id))
        (pixel-editor-put editor :palette-rowlength 32)
        (pixel-editor-put editor :palette-rowheight 24)
        (pixel-editor-put editor :palette-background (nth 0 (plist-get palette :colors)))
        (pixel-editor-put editor :palette-foreground (car (last (plist-get palette :colors))))
        (pixel-editor-put editor :bitmap-id (plist-get bitmap :id))
        (pixel-editor-put editor :bitmap-type (plist-get bitmap :type))
        (pixel-editor-put editor :bitmap-comma (plist-get bitmap :comma))
        (pixel-editor-put editor :bitmap-open (plist-get bitmap :open))
        (pixel-editor-put editor :bitmap-close (plist-get bitmap :close))
        (pixel-editor-put editor :bitmap-height (plist-get bitmap :height))
        (pixel-editor-put editor :bitmap-width (plist-get bitmap :width))
        (pixel-editor-put editor :bitmap-stride (plist-get bitmap :stride))
        (pixel-editor-put editor :bitmap-format (plist-get bitmap :format))
        (dolist (key pixel-editor-overlays)
          (cond ((eq :ov-complete key)
                 (setq ov-complete (make-overlay first-pos (+ last-pos 1)))
                 (plist-put editor :ov-complete ov-complete))
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
                 (plist-put editor :ov-editor ov-editor))
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
                 (plist-put editor :ov-source ov-source)
                 (plist-put editor :ov-array ov-array)
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
                   (plist-put editor key ov)
                   (setq last-pos next-pos)))))
        (overlay-put ov-complete
                     'pixel-editor editor)))))

(defun pixel-canvas-point (editor x y)
  (let ((w (pixel-editor-get editor :bitmap-width))
        (h (pixel-editor-get editor :bitmap-height))
        (ov (pixel-editor-get editor :ov-canvas)))
    (+ (overlay-start ov) 1 (* y 2) (* y w) x)))

(defun pixel-source-color (editor color)
  (cond ((string-equal (pixel-editor-get editor :bitmap-format) "palette")
         (let ((ov-palette (pixel-editor-get editor :ov-palette)))
           (gethash color (overlay-get ov-palette 'pixel-color-map))))
        ((or (string-equal (pixel-editor-get editor :bitmap-format) "rgb")
             (string-equal (pixel-editor-get editor :bitmap-format) "rgba"))
         (let* ((comma (pixel-editor-get editor :bitmap-comma))
                (type (pixel-editor-get editor :bitmap-type)))
           (mapconcat (lambda (c)
                        (prin1-to-string (cond ((string-equal type "float")
                                                c)
                                               ((string-equal type "int")
                                                (truncate (* c 255))))))
                      (color-name-to-rgb color)
                      comma)))))

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
              (replace-match (concat color (match-string 2)) 1))))))))

;; (pixel-source-replace-pixel (pixel-find-editor :id "lisp") 0 4 "1")

;; (let* ((editor (pixel-find-editor :id "single"))
;;        (comma (pixel-editor-get editor :bitmap-comma))
;;        (type (pixel-editor-get editor :bitmap-type))
;;        (color (mapconcat (lambda (c)
;;                            (prin1-to-string (cond ((string-equal type "float")
;;                                                    c)
;;                                                   ((string-equal type "int")
;;                                                    (truncate (* c 255))))))
;;                          (color-name-to-rgb "#aaaaaa")
;;                          comma)))
;;   (pixel-source-replace-pixel (pixel-find-editor :id "single") 2 2 color))

(defun pixel-tool-draw (input editor x0 y0 &optional x1 y1)
  (list (list x0 y0 (cond ((eq input 'mouse1)
                           (pixel-editor-get editor :palette-foreground))
                          ((eq input 'mouse3)
                           (pixel-editor-get editor :palette-background))))))

(defun pixel-canvas-update (editor updates)
  (dolist (u updates)
    (let ((x (nth 0 u))
          (y (nth 1 u))
          (color (nth 2 u))
          (zoomlevel (pixel-editor-get editor :editor-zoomlevel)))
      (put-text-property (pixel-canvas-point editor x y)
                         (1+ (pixel-canvas-point editor x y))
                         'display
                         (pixel-make-pixel color (* zoomlevel 2)))
      (pixel-source-replace-pixel editor x y (pixel-source-color editor color)))))

(defun pixel-canvas-action (input editor x0 y0 &optional x1 y1)
  (pixel-canvas-update editor
                       (cond ((eq (pixel-editor-get editor :tool-current) 'draw)
                              (pixel-tool-draw input editor x0 y0 x1 x1))))
  nil)

(defun pixel-make-canvas-action (input editor x y)
  (eval `(lambda (&optional pos)
           (interactive)
           (pixel-canvas-action (quote ,input) (quote ,editor) ,x ,y))))

(defun pixel-make-canvas-drag (input editor)
  (eval `(lambda (event)
           (interactive "e")
           (when event
             (let* ((a (nth 5 (nth 1 event)))
                    (b (nth 5 (nth 2 event)))
                    (x0 (when a (get-text-property a 'pixel-x)))
                    (y0 (when a (get-text-property a 'pixel-y)))
                    (x1 (when b (get-text-property b 'pixel-x)))
                    (y1 (when b (get-text-property b 'pixel-y))))
               (when (and x0 y0 x1 y1)
                 (message "%d %d %d %d" x0 y0 x1 y1)
                 (pixel-canvas-action (quote ,input) (quote ,editor) x0 y0 x1 y1)))))))

(defun pixel-editor-canvas-keymap (editor x y)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<RET>") (pixel-make-canvas-action 'keyboard editor x y))
    (define-key map (kbd "<SPC>") (pixel-make-canvas-action 'keyboard editor x y))
    (define-key map (kbd "<double-mouse-1>") (pixel-make-canvas-action 'mouse1 editor x y))
    (define-key map (kbd "<double-mouse-2>") (pixel-make-canvas-action 'mouse2 editor x y))
    (define-key map (kbd "<double-mouse-3>") (pixel-make-canvas-action 'mouse3 editor x y))
    (define-key map (kbd "<down-mouse-1>") (pixel-make-canvas-action 'mouse1 editor x y))
    (define-key map (kbd "<down-mouse-2>") (pixel-make-canvas-action 'mouse2 editor x y))
    (define-key map (kbd "<down-mouse-3>") (pixel-make-canvas-action 'mouse3 editor x y))
    (define-key map (kbd "<drag-mouse-1>") (pixel-make-canvas-drag 'mouse1 editor))
    (define-key map (kbd "<drag-mouse-2>") (pixel-make-canvas-drag 'mouse2 editor))
    (define-key map (kbd "<drag-mouse-3>") (pixel-make-canvas-drag 'mouse3 editor))
    map))

;; (mouse-set-point event)
;; (let (event)
;;   (track-mouse
;;     (while (progn
;;              (setq event (read-event))
;;              (or (mouse-movement-p event)
;;                  (memq (car-safe event) '(switch-frame select-window))))
;;       (mouse-set-point event))))

(defun pixel-palette-action (input editor &optional color)
  (pixel-editor-put editor :palette-foreground color)
  nil)

(defun pixel-make-palette-action (input editor &optional color)
  (eval `(lambda (&optional pos)
           (interactive)
           (pixel-palette-action (quote ,input) (quote ,editor) ,color))))

(defun pixel-make-palette-drag (input editor &optional color)
  (eval `(lambda (event)
           (interactive "e")
           (pixel-palette-action (quote ,input) (quote ,editor) ,color))))

(defun pixel-editor-palette-keymap (editor &optional color)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<RET>") (pixel-make-palette-action 'keyboard editor color))
    (define-key map (kbd "<SPC>") (pixel-make-palette-action 'keyboard editor color))
    (define-key map (kbd "<down-mouse-1>") (pixel-make-palette-action 'mouse1 editor color))
    (define-key map (kbd "<down-mouse-2>") (pixel-make-palette-action 'mouse2 editor color))
    (define-key map (kbd "<down-mouse-3>") (pixel-make-palette-action 'mouse3 editor color))
    (define-key map (kbd "<drag-mouse-1>") (pixel-make-palette-drag 'mouse1 editor color))
    (define-key map (kbd "<drag-mouse-2>") (pixel-make-palette-drag 'mouse2 editor color))
    (define-key map (kbd "<drag-mouse-3>") (pixel-make-palette-drag 'mouse3 editor color))
    map))


(defvar pixel-editor-state (make-hash-table :test 'equal))

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

(defun pixel-editor-get (editor &optional prop default)
  (or (plist-get editor prop)
      (let* ((key (plist-get editor :id)))
        (when key
          (or (unless prop (gethash key pixel-editor-state))
              (plist-get (or (gethash key pixel-editor-state)
                             (puthash key '() pixel-editor-state)) prop))))
      default))

;; (pixel-editor-get (list :id "aaa" :foo "lala") :ficken)
(provide 'pixel-editor)
