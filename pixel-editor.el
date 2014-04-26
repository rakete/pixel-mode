;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(require 'pixel-bitmap nil 'noerror)

(defun pixel-editor-insert-tools (editor &rest tools))

(defun* pixel-find-index (x xs &key (test'eq))
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
             (bg (pixel-editor-get editor :background))
             (fg (pixel-editor-get editor :foreground))
             (indentation (pixel-editor-get editor :indentation))
             (rowlength (pixel-editor-get editor :palette-rowlength))
             (rowheight (pixel-editor-get editor :palette-rowheight))
             (p (overlay-start ov))
             (colors (plist-get palette :colors))
             (template (pixel-xpm-data (pixel-make-bitmap :width rowheight :height rowheight :background 0)))
             (whitespace (pixel-make-icon :type 'xpm
                                          :data (pixel-xpm-data (pixel-make-bitmap :width indentation :height rowheight :background 0))
                                          :color-symbols (pixel-xpm-colors (pixel-make-palette bg bg))
                                          :height rowheight))
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
                 (icon (pixel-make-icon :type 'xpm
                                        :data template
                                        :color-symbols (pixel-xpm-colors (pixel-make-palette c fg))
                                        :height rowheight))
                 (hover-face (pixel-make-face "pixel-mode-palette-hover-face" (color-complement-hex avg) c)))
            (puthash c n color-map)
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
             (bg (pixel-editor-get editor :background))
             (zoomlevel (pixel-editor-get editor :zoomlevel))
             (indentation (pixel-editor-get editor :indentation))
             (w (plist-get bitmap :width))
             (h (plist-get bitmap :height))
             (p (overlay-start ov))
             (colors (apply 'vector (plist-get palette :colors)))
             (avg (pixel-palette-average palette))
             (whitespace (pixel-make-icon :type 'xpm
                                          :data (pixel-xpm-data (pixel-make-bitmap :width indentation :height (* zoomlevel 2) :background 0))
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
                              'pixel-occupied id
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
                                  'pixel-occupied id
                                  'pixel-canvas t
                                  'pixel-color c
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
                                              (let ((template (pixel-xpm-data (pixel-make-bitmap :width (* zoomlevel 2) :height (* zoomlevel 2) :background 0))))
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
        (pixel-editor-put editor :background background)
        (pixel-editor-put editor :foreground foreground)
        (pixel-editor-put editor :zoomlevel 12)
        (pixel-editor-put editor :indentation 20)
        (pixel-editor-put editor :palette-id (plist-get bitmap :palette-id))
        (pixel-editor-put editor :palette-rowlength 32)
        (pixel-editor-put editor :palette-rowheight 24)
        (pixel-editor-put editor :bitmap-id (plist-get bitmap :id))
        (pixel-editor-put editor :bitmap-type (plist-get bitmap :type))
        (pixel-editor-put editor :bitmap-comma (plist-get bitmap :comma))
        (pixel-editor-put editor :bitmap-open (plist-get bitmap :open))
        (pixel-editor-put editor :bitmap-close (plist-get bitmap :close))
        (pixel-editor-put editor :bitmap-height (plist-get bitmap :height))
        (pixel-editor-put editor :bitmap-width (plist-get bitmap :width))
        (pixel-editor-put editor :bitmap-stride (plist-get bitmap :stride))
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

(defun pixel-image-put-color (image prop val)
  `(image ,(plist-put (cdr image) prop val)))

(defun pixel-canvas-point (editor x y)
  (let ((w (pixel-editor-get editor :bitmap-width))
        (h (pixel-editor-get editor :bitmap-height))
        (ov (pixel-editor-get editor :ov-canvas)))
    (+ (overlay-start ov) 1 (* y 2) (* y w) x)))

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

;; (replace-match)

(defun pixel-palette-color-id (editor color)
  (let ((ov-palette (pixel-editor-get editor :ov-palette)))
    (gethash color (overlay-get ov-palette 'pixel-color-map))))

;; [ 0, 1, 2,
;;   3, 4, 5,
;;   6, 7, 8 ]
(defun pixel-source-replace (editor x y color)
  (let* ((width (pixel-editor-get editor :bitmap-width))
         (height (pixel-editor-get editor :bitmap-height))
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
                            (re-search-forward (concat "[^0-9.]*\\(?:[0-9.]+[^0-9.]*\\)\\{" (prin1-to-string width) "\\}")))
                    lastx 0))
            (setq lastx (dotimes (xi (- x lastx) (+ lastx xi))
                          (re-search-forward "[0-9.]+")))
            (save-excursion
              (re-search-forward "\\([0-9.]+\\)")
              ;;(print (match-string-no-properties 1))
              (replace-match color 1))))))))

;; (pixel-source-replace (pixel-find-editor :id "lisp") 0 0 "1")
;; (pixel-source-replace (pixel-single-editor :id "single") 0 0 "#00ff00")

;; (let ((foo ""))
;;   (dotimes (y 2 foo)
;;     (setq foo (concat foo (mapconcat 'prin1-to-string (print (butlast (nthcdr (* 4 (print 1)) '(0 1 2 3 4 5 6 7)) 4)) ", ")))))

;; (let ((xs '(0 1 2 3 4 5 6 7))
;;       (w 4)
;;       (x 0))
;;   (loop for n from 0 below (length xs)
;;         do (setq x (1+ x))
;;         collect (concat (prin1-to-string (nth n xs))
;;                         (cond ((eq x w)
;;                                (progn
;;                                  (setq x 0)
;;                                  ",<eof>"))
;;                               ((eq n (1- (length xs)))
;;                                "<eof>")
;;                               (t
;;                                ", ")))))

;; (mapcar 'prin1-to-string '(0 1 2 3 4 5 6 7))

;;(pixel-source-replace nil '(1 2 3) '(4 5 8 9 10) '(6 7))

(defun pixel-canvas-draw (editor x y zoomlevel color)
  (put-text-property (pixel-canvas-point editor x y)
                     (1+ (pixel-canvas-point editor x y))
                     'display
                     (pixel-make-pixel 'xpm zoomlevel color)))

(defun pixel-canvas-action (input editor &optional x y)
  (pixel-canvas-draw editor x y
                     (pixel-editor-get editor :zoomlevel)
                     (pixel-editor-get editor :foreground))
  (print (pixel-palette-color-id editor (pixel-editor-get editor :foreground)))
  nil)

(defun pixel-make-canvas-action (input editor &optional x y)
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
  (pixel-editor-put editor :foreground color)
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
