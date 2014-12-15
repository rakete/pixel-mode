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

(defun pixel-copy-hover-face (face &optional hex x y)
  (print (face-attribute face :box t 'default))
  (let* ((sym (intern (cond ((and x y)
                             (replace-regexp-in-string "^.*\\(|[0-9]+|[0-9]+\\)$" (concat "|" (prin1-to-string x) "|" (prin1-to-string y)) (prin1-to-string face) nil nil 1))
                            ((stringp hex)
                             (replace-regexp-in-string "^.*\\(|[0-9a-fA-F]+\\)$" (concat "|" (replace-regexp-in-string "#" "" hex)) (prin1-to-string face) nil nil 1)))))
         (new-face (make-face sym)))
    (set-face-attribute new-face nil :box (print (face-attribute face :box t 'default)))
    new-face))

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

(defun pixel-map-text-properties (start end props f &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (forward-char)
        (let ((plist '()))
          (dolist (prop props)
            (setq plist (plist-put plist prop (get-text-property (point) prop))))
          (when (every #'identity plist)
            (setq plist (funcall f (point) plist))
            (add-text-properties (point) (1+ (point)) plist)))))))

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

(defun pixel-editor-insert-toolbar (editor &rest tools)
  (pixel-editor-put editor :tool-current 'pixel-tool-draw))

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
             ;;(id (plist-get editor :id))
             (color-map (make-hash-table :test 'equal)))
        (goto-char start)
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
                 (hover-face (pixel-make-hover-face "pixel-mode-palette-hover-face" n 0)))
            (puthash c (nth n symbols) color-map)
            (insert (propertize (if (string-equal c (car (last colors)))
                                    (propertize " " 'intangible 'editor) ;;
                                  (propertize " "))
                                'display icon
                                'pixel-occupied id
                                'pixel-palette-color c
                                'pixel-palette-id n
                                'pixel-palette t
                                'mouse-face hover-face
                                'keymap (pixel-make-palette-keymap editor c)))))
        (overlay-put ov 'pixel-color-map color-map)))))

(defun pixel-editor-insert-canvas (editor bitmap)
  (when (and editor bitmap)
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
             (keymap (pixel-make-canvas-keymap editor)))
        (delete-region (overlay-start ov) (overlay-end ov))
        (goto-char start)
        (dotimes (y h)
          (insert (propertize " "
                              'intangible 'editor
                              'display whitespace
                              'pixel-occupied id
                              'pixel-canvas t))
          (dotimes (x w)
            (let* ((v (pixel-bitmap-ref bitmap x y))
                   (c (elt colors v))
                   (a (elt alphas v))
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
                                  'keymap keymap
                                  ))))
          (insert (propertize "\n"
                              'intangible 'editor
                              'pixel-occupied id
                              'pixel-canvas t
                              'line-height t
                              'line-spacing nil))
          (move-overlay ov start (point)))))))

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
        (pixel-editor-put editor :editor-zoomlevel 8)
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
                      (nconc (color-name-to-rgb color) (when alpha (list alpha)))
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
              (replace-match (concat color (match-string 2)) t nil nil 1))))))))

;;
;; Canvas Tools
;;

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


(defun pixel-canvas-point (editor x y)
  (let ((w (pixel-editor-get editor :bitmap-width))
        (h (pixel-editor-get editor :bitmap-height))
        (ov (pixel-editor-get editor :ov-canvas)))
    (+ (overlay-start ov) 1 (* y 2) (* y w) x)))

(defun pixel-canvas-merge (editor state)
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

;; (defun* pixel-canvas-layer (editor state)
;;   ;; (when (plist-get state :layer)
;;   ;;   (dolist (ov (plist-get state :layer))
;;   ;;     (delete-overlay ov)))
;;   (let ((updates (copy-list (plist-get state :updates))))
;;     (print (sort (print updates)
;;                  (lambda (a b)
;;                    (if (eq (nth 1 a) (nth 1 b))
;;                        (< (nth 0 a) (nth 0 b))
;;                      (< (nth 1 a) (nth 1 b)))
;;                    ))))
;;   state)

(defun pixel-canvas-refresh (editor)
  (let* ((p (point))
         (id (pixel-editor-get editor :id))
         (ov-source (pixel-editor-get editor :ov-source))
         (auto-window-vscroll nil)
         (scroll-conservatively 10000)
         (scroll-step 1)
         (scroll-margin 1)
         (scroll-conservatively 0)
         (scroll-up-aggressively 0.01)
         (scroll-down-aggressively 0.01)
         (deactivate-mark nil)
         )
    (save-excursion
      (save-restriction
        (goto-char (overlay-start ov-source))
        (pixel-toggle-editor :id id)
        (pixel-toggle-editor :id id)
        ))
    (goto-char p)
    nil))

(defun pixel-canvas-replace (editor bitmap)
  (with-current-buffer (pixel-editor-buffer editor)
    (save-excursion
      (let* ((ov-array (pixel-editor-get editor :ov-array))
             (ov-source (pixel-editor-get editor :ov-source))
             (id (pixel-editor-get editor :bitmap-id))
             (new-palette (plist-get bitmap :palette))
             (new-format (plist-get bitmap :format))
             (new-type (plist-get bitmap :type))
             (new-width (plist-get bitmap :width))
             (new-height (plist-get bitmap :height))
             (new-stride (plist-get bitmap :stride))
             (new-array (plist-get bitmap :array))
             (source (buffer-substring (overlay-start ov-source) (overlay-end ov-source)))
             (comma (pixel-find-comma source)))
        (goto-char (overlay-start ov-source))
        (looking-at (pixel-regex :bitmap id :id id))
        (when (and new-palette
                   (match-string 2))
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
                 ;;(new-width 4)
                 ;;(new-height 4)
                 ;;(new-array (make-vector 9 2))
                 (new-array-string (mapconcat #'identity
                                              (loop for i from 0 below (* new-width new-height)
                                                    do (if (< w new-width)
                                                           (setq w (1+ w))
                                                         (setq w 1)
                                                         (setq h (1+ h)))
                                                    collect (cond
                                                             ((and (eq w new-width) (< h (1- new-height)))
                                                              (concat (prin1-to-string (condition-case nil (elt new-array i) (error 0))) "\n"))
                                                             (t
                                                              (prin1-to-string (condition-case nil (elt new-array i) (error 0))))))
                                              " ")))
            ;;new-array-string
            (replace-match new-array-string t nil nil 9)
            ))
        (pixel-canvas-refresh editor)
        ))))

(defun pixel-canvas-resize (editor add-width add-height)
  (let* ((id (pixel-editor-get editor :id))
         (bitmap (pixel-bitmap-resize (pixel-find-bitmap :id id) add-width add-height))
         (l (line-number-at-pos)))
    (pixel-canvas-replace editor bitmap)
    (goto-line (+ l add-height))))

;;
;; Action Utilities
;;

(defun pixel-make-canvas-click (input type editor)
  (lambda (&optional pos)
    (interactive)
    (let* ((x (get-text-property (point) 'pixel-x))
           (y (get-text-property (point) 'pixel-y))
           (color (get-text-property (point) 'pixel-color))
           (alpha (get-text-property (point) 'pixel-alpha))
           (tool (pixel-editor-get editor :tool-current)))
      (pixel-canvas-merge editor (funcall tool input type editor x y color alpha)))))

(defun pixel-make-canvas-motion (input type editor)
  (lambda (event)
    (interactive "e")
    (let* ((tool (pixel-editor-get editor :tool-current))
           (p (posn-point (event-start event)))
           (x (get-text-property p 'pixel-x))
           (y (get-text-property p 'pixel-y))
           (color (get-text-property p 'pixel-color))
           (alpha (get-text-property p 'pixel-alpha))
           (state (funcall tool input type editor x y color alpha))
           (last-x x)
           (last-y y)
           (layer nil))
      (track-mouse
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
              (setq state (pixel-canvas-layer editor
                                              (funcall tool input type editor
                                                       x y color alpha
                                                       drag-x drag-y drag-color drag-alpha
                                                       state))
                    last-x drag-x
                    last-y drag-y)
              ))))
      (pixel-canvas-merge editor state))))

(defun pixel-make-canvas-keymap (editor)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<RET>") (pixel-make-canvas-click 'keyboard 'single editor))
    (define-key map (kbd "<SPC>") (pixel-make-canvas-click 'keyboard 'single editor))
    (define-key map (kbd "<double-mouse-1>") (pixel-make-canvas-motion 'mouse1 'double editor))
    (define-key map (kbd "<double-mouse-2>") (pixel-make-canvas-motion 'mouse2 'double editor))
    (define-key map (kbd "<double-mouse-3>") (pixel-make-canvas-motion 'mouse3 'double editor))
    (define-key map (kbd "<down-mouse-1>") (pixel-make-canvas-motion 'mouse1 'single editor))
    (define-key map (kbd "<down-mouse-2>") (pixel-make-canvas-motion 'mouse2 'single editor))
    (define-key map (kbd "<down-mouse-3>") (pixel-make-canvas-motion 'mouse3 'single editor))
    (define-key map (kbd "S-<right>") (lambda (&optional pos) (interactive) (pixel-canvas-resize editor 1 0)))
    (define-key map (kbd "S-<left>") (lambda (&optional pos) (interactive) (pixel-canvas-resize editor -1 0)))
    (define-key map (kbd "S-<down>") (lambda (&optional pos) (interactive) (pixel-canvas-resize editor 0 1)))
    (define-key map (kbd "S-<up>") (lambda (&optional pos) (interactive) (pixel-canvas-resize editor 0 -1)))
    map))

(defun pixel-make-palette-click (input type editor color)
  (lambda (&optional pos)
    (interactive)
    (pixel-editor-put editor :palette-foreground color)))

(defun pixel-make-palette-keymap (editor color)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<RET>") (pixel-make-palette-click 'keyboard 'single editor color))
    (define-key map (kbd "<SPC>") (pixel-make-palette-click 'keyboard 'single editor color))
    (define-key map (kbd "<down-mouse-1>") (pixel-make-palette-click 'mouse1 'single editor color))
    (define-key map (kbd "<down-mouse-2>") (pixel-make-palette-click 'mouse2 'single editor color))
    (define-key map (kbd "<down-mouse-3>") (pixel-make-palette-click 'mouse3 'single editor color))
    map))

(provide 'pixel-editor)
