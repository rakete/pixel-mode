(require 'pixel-palette)
(require 'pixel-bitmap)

(defun pixel-editor-find-free-point (ov)
  (let* ((start (overlay-start ov))
         (end (overlay-end ov))
         (p start))
    (while (and (get-text-property p 'pixel-occupied)
                (< p end))
      (setq p (1+ p)))
    p))

(defun pixel-editor-insert-tools (editor &rest tools))

(defun pixel-editor-insert-palette (editor palette)
  (when (and editor palette)
    (save-excursion
      (let* ((ov (plist-get editor :ov-palette))
             (bg (plist-get editor :background))
             (fg (plist-get editor :foreground))
             (rowlength (plist-get editor :palette-rowlength))
             (rowheight (plist-get editor :palette-rowheight))
             (indentation (plist-get editor :indentation))
             (p (pixel-editor-find-free-point ov))
             (colors (plist-get palette :colors))
             (template (pixel-xpm-data (pixel-make-bitmap :w rowheight :h rowheight :background 0)))
             (whitespace (pixel-make-icon :type 'xpm
                                          :data (pixel-xpm-data (pixel-make-bitmap :w indentation :h rowheight :background 0))
                                          :color-symbols (pixel-xpm-colors (pixel-make-palette bg bg))
                                          :height rowheight))
             (avg (pixel-palette-average palette))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t)
             (id (plist-get editor :id)))
        (goto-char p)
        (when (<= (- (point) (point-at-bol)) 0)
          (insert (propertize " "
                              'intangible 'editor
                              'display whitespace
                              'pixel-occupied t
                              'palette t)))
        (dolist (c colors)
          (when (and rowlength (>= (- (point) (point-at-bol)) (+ (if indentation 1 0) rowlength)))
            (insert (propertize "\n"
                                'intangible 'editor
                                'pixel-occupied t
                                'pixel-palette t
                                ;;'line-height t
                                ;;'line-spacing nil
                                ))
            (insert (propertize " "
                                'intangible 'editor
                                'display whitespace
                                'pixel-occupied t
                                'pixel-palette t)))
          (let* ((icon (pixel-make-icon :type 'xpm
                                        :data template
                                        :color-symbols (pixel-xpm-colors (pixel-make-palette c fg))
                                        :height rowheight))
                 (hover-face (pixel-make-face "pixel-mode-palette-hover-face" (color-complement-hex avg) c)))
            (insert (propertize (if (string-equal c (car (last colors)))
                                    (propertize " " 'intangible 'editor) ;; 
                                  (propertize " "))
                                'display icon
                                'pixel-occupied t
                                'pixel-palette t
                                'mouse-face hover-face
                                ;;'follow-link (pixel-make-palette-action 'mouse1 id c)
                                'keymap (pixel-make-palette-keymap id c)))))))))

(defun pixel-editor-insert-canvas (editor palette bitmap)
  (when (and editor palette bitmap)
    (save-excursion
      (let* ((ov (plist-get editor :ov-canvas))
             (bg (plist-get editor :background))
             (zoomlevel (plist-get editor :zoomlevel))
             (indentation (plist-get editor :indentation))
             (w (plist-get bitmap :w))
             (h (plist-get bitmap :h))
             (p (pixel-editor-find-free-point ov))
             ;;(template (pixel-xpm-data (pixel-make-bitmap :w (* zoomlevel 2) :h (* zoomlevel 2) :background 0)))
             (pixel-cache (make-hash-table :test 'equal :size (* w h)))
             (colors (apply 'vector (plist-get palette :colors)))
             (avg (pixel-palette-average palette))
             (whitespace (pixel-make-icon :type 'xpm
                                          :data (pixel-xpm-data (pixel-make-bitmap :w indentation :h (* zoomlevel 2) :background 0))
                                          :color-symbols (pixel-xpm-colors (pixel-make-palette bg))
                                          :height (* zoomlevel 2)))
             (id (plist-get bitmap :id))
             (inhibit-point-motion-hooks t)
             (disable-point-adjustment t))
        (goto-char p)
        (dotimes (y h)
          (insert (propertize " "
                              'intangible 'editor
                              'display whitespace
                              'pixel-occupied t
                              'pixel-canvas t))
          (dotimes (x w)
            (let* ((v (pixel-bitmap-ref bitmap x y))
                   (c (elt colors v))
                   (pixel (pixel-make-pixel 'xpm zoomlevel c))
                   (hover-face (pixel-make-face "pixel-mode-canvas-hover-face" (color-complement-hex avg) nil x y)))
              (insert (propertize (if (eq x (- w 1))
                                      (propertize " " 'intangible 'editor) 
                                    (propertize " "))
                                  'display pixel
                                  'pixel-occupied t
                                  'pixel-value v
                                  'pixel-color c
                                  'pixel-canvas t
                                  'pixel-x x
                                  'pixel-y y
                                  'line-height t
                                  'line-spacing nil
                                  'mouse-face hover-face
                                  ;;'follow-link (pixel-make-bitmap-action 'mouse1 id x y)
                                  'keymap (pixel-make-canvas-keymap id x y)))))
          (insert (propertize "\n"
                              'intangible 'editor
                              'pixel-occupied t
                              'pixel-bitmap t
                              'line-height t
                              'line-spacing nil
                              )))))))

(defun pixel-make-icon (&rest keys)
  (find-image (list keys)))

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

(defvar pixel-pixel-cache (make-hash-table :test 'equal))

(defun pixel-make-pixel (type zoomlevel color)
  (let ((key (format "%s-%d-%s" (prin1-to-string type) zoomlevel (replace-regexp-in-string "#" "" color))))
    (cons 'image (cdr (or (gethash key pixel-pixel-cache nil)
                          (puthash key (cond ((eq type 'xpm)
                                              (let ((template (pixel-xpm-data (pixel-make-bitmap :w (* zoomlevel 2) :h (* zoomlevel 2) :background 0))))
                                                (pixel-make-icon :type 'xpm
                                                                 :data template
                                                                 :color-symbols (pixel-xpm-colors (pixel-make-palette color))
                                                                 :height (* zoomlevel 2)))))
                                   pixel-pixel-cache))))))

;; (setq pixel-editor-overlays
;;       '(:ov-complete :ov-source :ov-editor :ov-seperator1 :ov-tools :ov-seperator2 :ov-palette :ov-seperator3 :ov-canvas))

;; (setq pixel-editor-overlays
;;       '(:ov-complete :ov-source :ov-editor :ov-seperator2 :ov-palette :ov-seperator3 :ov-canvas))

(defvar pixel-editor-overlays
  '(:ov-complete :ov-editor :ov-seperator2 :ov-palette :ov-seperator3 :ov-canvas))

(defun pixel-editor-remove (editor)
  (let ((modified-state (buffer-modified-p)))
    (dolist (key pixel-editor-overlays)
      (when (eq key :ov-editor)
        (delete-region (overlay-start (plist-get editor key))
                       (overlay-end (plist-get editor key))))
      (delete-overlay (plist-get editor key)))
    (unless (find :ov-source pixel-editor-overlays)
      (delete-overlay (plist-get editor :ov-source)))
    (set-buffer-modified-p modified-state)))

;; (pixel-editor-create (pixel-find-palette :id "bnw") (pixel-find-bitmap :id "test1") (pixel-find-bitmap :find-origin t :id "test1")
;;                      :background "#2f2f2f"
;;                      :foreground "#ffffff"
;;                      :source-background "#222222")

(defun* pixel-editor-create (palette bitmap origin &key (background nil) (foreground nil) (source-background nil))
  (when (and palette bitmap origin)
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
             ;; when :ov-source is not in pixel-editor-overlays, we create a overlay here and hide the text
             (ov-source (unless (find :ov-source pixel-editor-overlays)
                          (let ((ov (make-overlay first-pos src-end)))
                            (overlay-put ov 'invisible t)
                            ;;(overlay-put ov 'face `((:background "#ff0000")))
                            ov)))
             ;; need overlay symbol so we can add the editor as property to it
             (ov-complete nil)
             (ov-editor nil)
             (src-text (unless (overlayp ov-source)
                         (let ((str (buffer-substring first-pos src-end)))
                           (delete-region first-pos src-end)
                           str)))
             (editor (list :id (plist-get bitmap :id)
                           :palette-id (plist-get bitmap :palette-id)
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
                                       (point-at-bol))
                       bg source-background)
                 (setq ov-source (make-overlay last-pos next-pos))
                 (overlay-put ov-source 'face `((:background ,source-background)
                                                (:foreground ,foreground)))
                 (plist-put editor :ov-source ov-source)
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

(defun pixel-canvas-action (input id &optional x y pos)
  (print (format "%s %s %d %d" (prin1-to-string input) id x y))
  nil)

(defun pixel-make-canvas-action (input id &optional x y)
  (eval `(lambda (&optional pos)
           (interactive)
           (pixel-canvas-action (quote ,input) ,id ,x ,y pos))))

(defun pixel-make-canvas-keymap (id &optional x y)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<RET>") (pixel-make-canvas-action 'keyboard id x y))
    (define-key map (kbd "<SPC>") (pixel-make-canvas-action 'keyboard id x y))
    (define-key map (kbd "<double-mouse-1>") (pixel-make-canvas-action 'mouse1 id x y))
    (define-key map (kbd "<double-mouse-2>") (pixel-make-canvas-action 'mouse2 id x y))
    (define-key map (kbd "<double-mouse-3>") (pixel-make-canvas-action 'mouse3 id x y))
    (define-key map (kbd "<down-mouse-1>") (pixel-make-canvas-action 'mouse1 id x y))
    (define-key map (kbd "<down-mouse-2>") (pixel-make-canvas-action 'mouse2 id x y))
    (define-key map (kbd "<down-mouse-3>") (pixel-make-canvas-action 'mouse3 id x y))
    (define-key map (kbd "<drag-mouse-1>") 'pixel-canvas-drag)
    (define-key map (kbd "<drag-mouse-2>") 'pixel-canvas-drag)
    (define-key map (kbd "<drag-mouse-3>") 'pixel-canvas-drag)
    map))

(defun pixel-canvas-drag (event)
  (interactive "e")
  (print event))

;; (mouse-set-point event)
;; (let (event)
;;   (track-mouse
;;     (while (progn
;;              (setq event (read-event))
;;              (or (mouse-movement-p event)
;;                  (memq (car-safe event) '(switch-frame select-window))))
;;       (mouse-set-point event))))

(defun pixel-palette-action (input id &optional color pos)
  (print (format "%s %s %s" (prin1-to-string input) id color))
  nil)

(defun pixel-make-palette-action (input id &optional color)
  (eval `(lambda (&optional pos)
           (interactive)
           (pixel-palette-action (quote ,input) ,id ,color pos))))

(defun pixel-make-palette-keymap (id &optional color)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<RET>") (pixel-make-palette-action 'keyboard id color))
    (define-key map (kbd "<SPC>") (pixel-make-palette-action 'keyboard id color))
    (define-key map (kbd "<down-mouse-1>") (pixel-make-palette-action 'mouse1 id color))
    (define-key map (kbd "<down-mouse-2>") (pixel-make-palette-action 'mouse2 id color))
    (define-key map (kbd "<down-mouse-3>") (pixel-make-palette-action 'mouse3 id color))
    (define-key map (kbd "<drag-mouse-1>") 'pixel-palette-drag)
    (define-key map (kbd "<drag-mouse-2>") 'pixel-palette-drag)
    (define-key map (kbd "<drag-mouse-3>") 'pixel-palette-drag)
    map))

(defun pixel-palette-drag (event)
  (interactive "e")
  (print event))

(provide 'pixel-editor)
