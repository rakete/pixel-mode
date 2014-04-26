
(require 'cl)
(require 'color)

(require 'pixel-editor nil 'noerror)

(defvar pixel-types-regex "\\(int\\|float\\)?")

(defun* pixel-regex (&key (bitmap nil) (palette nil) (id nil) (mm nil) (quick nil))
  (unless mm
    (setq mm major-mode))
  (unless (or palette bitmap)
    (setq palette t
          bitmap t))
  (let* ((palette-id (or (and (stringp palette) palette) "[^ \t\n]+"))
         (init (cond ((or (stringp palette) (and palette bitmap)) "\\(?:\\<palette\\>\\|\\<bitmap\\>\\)")
                     (palette "\\<palette\\>")
                     (bitmap "\\<bitmap\\>")))
         (palette-optional (and (or (not (stringp palette))
                                    (string-equal palette id))
                                "?"))
         (id (if id
                 (regexp-quote id)
               "[^ \t\n]+"))
         (quick-re (concat "^[ \t/\*;#%]*" init ":[ \t]*\\(" id "\\)\n"
                           ;; comment   palette/bitmap   1:specific or any name
                           "\\(?:[ \t/\*;#%]*palette:[ \t]*\\(" palette-id "\\)" "[ \t\n]+" "\\)" palette-optional
                           ;; comment, 2:specific or any palette name when init=bitmap, whole thing may be optional
                           ;; a bitmap without a using param can be itself a palette, when it has colors packed in (rgb triples)
                           "\\(?:[ \t/\*;#%]*format:[ \t]*\\(" "[^ \t\n]+" "\\)" "[ \t\n]+" "\\)?"
                           ;; 3:format of the image (rgba or rgb, defaults to rgb)
                           ))
         (full-re (unless quick
                    (concat
                     "\\(?:" "[^\[\(\{\"]*" pixel-types-regex "[ \t]+[^\[]+\\[\\([0-9]+\\)\\*\\([0-9]+\\)\\(?:\\*\\([0-9]+\\)\\)?\\]" "\\)?"
                     ;; search for 4:types, if any, then search for c-style array indicator, getting 5:width, 6:height and optionally 7:number of components
                     "\\(?:.*\n\\)*?.*\\(\\[\\|\(\\|\{\\)" "[ \t\n]*?"
                     ;; 8:open
                     "\\(\\(?:[ \t/\*;#%]*\\(?:[0-9\\.]+\\|[0-9]+\\)\\(?:[ \t,]*\\)?\n?\\)+\\)"
                     ;; 9:array
                     ))))
    (concat quick-re full-re)))

(defun pixel-mapx (x f xs)
  (when (< x 1)
    (setq x 1))
  (let ((steps (ceiling (/ (float (length xs)) (float x)))))
    (loop for n from 0 below steps
          collect (apply f n (subseq xs (* n x) (+ (* n x) x))))))

;; (pixel-mapx 3 (lambda (&rest args) (print args)) '(1 2 3 4 5 6 7 8 9 10))

(defun pixel-find-comma (source)
  (save-match-data
    (string-match "^[^0-9\\.]*[0-9\\.]+\\([^0-9\\.]+\\)[0-9\\.]+.*" source)
    (match-string 1 source)))

;; (pixel-find-comma ";; { 0, 0, 0, 0, 0, 0, 0")

(defun pixel-find-width (source &optional comma)
  (save-match-data
    (let ((comma-re (concat "[" (regexp-quote (remove-duplicates (or comma (pixel-find-comma source)))) "]")))
      (string-match (concat "^[^0-9\\.]*\\(\\(?:[0-9\\.]+" comma-re "*\\)+\\)") source)
      (length (split-string (match-string 1 source) comma-re t)))))

;; (pixel-find-width ";; 0 , 0 , 0 , 0 , 0 ,  |0 0 0 0 0")

(defun pixel-find-height (source)
  (save-match-data
    (length (split-string source "\n"))))

;; (pixel-find-height ";; 0 , 0 , 0 , 0 , 0 ,  \n0 0 0 0 0")

(defun pixel-find-type (numbers)
  (if (some 'floatp numbers)
      "float"
    "int"))

;; (pixel-find-type '(0 0 0 0 0.1))

(defun pixel-read-bitmap-at-point (&optional point)
  (interactive)
  (save-excursion
    (when point
      (goto-char point))
    (when (thing-at-point-looking-at (pixel-regex :bitmap t :mm major-mode))
      (let* ((id (print (match-string-no-properties 1)))
             (palette-id (or (match-string-no-properties 2) id))
             (format (or (match-string-no-properties 3) "nil"))
             (array (print (match-string-no-properties 9)))
             (comma (print (pixel-find-comma array)))
             (stride (print (let ((c (or (read (or (match-string-no-properties 7) "nil"))
                                         (cond ((string-equal format "rgba")
                                                4)
                                               ((string-equal format "rgb")
                                                3)
                                               ((not (string-equal palette-id id))
                                                1)
                                               (t
                                                (pixel-find-stride array comma))))))
                              (cond ((and (eq c 4) (string-equal format "nil"))
                                     (progn (setq format "rgba") c))
                                    ((and (eq c 3) (string-equal format "nil"))
                                     (progn (setq format "rgb") c))
                                    ((and (eq c 1) (string-equal format "nil"))
                                     (progn (setq format "palette") c))
                                    (t c)))))
             (w (print (or (read (or (match-string-no-properties 5) "nil"))
                           (/ (pixel-find-width array comma) stride))))
             (h (print (or (read (or (match-string-no-properties 6) "nil"))
                           (pixel-find-height array))))
             (open (print (match-string-no-properties 8)))
             (numbers (print (read (concat "(" (save-match-data (replace-regexp-in-string "[^0-9\\.]" " " array)) ")"))))
             (type (or (print (match-string-no-properties 4))
                       (pixel-find-type numbers)))
             (d (- (print (length numbers)) (* w h stride)))
             (palette (when (string-equal id palette-id) (plist-get (plist-get (pixel-read-palette-at-point) :palette) :colors)))
             (alpha (make-vector (* w h) 1))
             (array (apply 'vector (pixel-mapx stride (lambda (n &rest color)
                                                   (cond ((eq (length color) 3)
                                                          (position (apply 'color-rgb-to-hex (pixel-normalize-color type color))
                                                                    palette :test 'equal))
                                                         ((eq (length color) 4)
                                                          (progn
                                                            (setf (elt alpha n) (nth 3 color))
                                                            (position (apply 'color-rgb-to-hex (butlast (pixel-normalize-color type color) 1))
                                                                      palette :test 'equal)))
                                                         (t (car color))))
                                               (append numbers (make-list d 0))))))
        (list :bitmap (list :id id
                            :palette-id palette-id
                            :format format
                            :type type
                            :width w
                            :height h
                            :stride stride
                            :open open
                            :comma comma
                            :array array
                            :alpha alpha
                            :close (cond ((string-equal open "(")
                                          ")")
                                         ((string-equal open "[")
                                          "]")
                                         ((string-equal open "{")
                                          "}")))
              :bitmap-origin (list :id id
                                   :beginning (match-beginning 0)
                                   :end (match-end 0)
                                   :array-beginning (match-beginning 9)
                                   :array-end (match-end 9)
                                   :buffer (current-buffer)))))))

(defun pixel-find-stride (source &optional comma)
  (let ((w (pixel-find-width source comma)))
    (cond ((eq (mod w 3) 2)
           4)
          ((eq (mod w 3) 1)
           nil)
          (t
           3))))

(defun pixel-read-palette-at-point (&optional point)
  (interactive)
  (save-excursion
    (when point
      (goto-char point))
    (when (thing-at-point-looking-at (pixel-regex :mm major-mode))
      (let* ((id (match-string-no-properties 1))
             (palette-id (match-string-no-properties 2))
             (format (or (match-string-no-properties 3) "nil"))
             (array (match-string-no-properties 9))
             (comma (pixel-find-comma array))
             (stride (print (let ((c (or (read (or (match-string-no-properties 7) "nil"))
                                         (cond ((string-equal format "rgba")
                                                4)
                                               ((string-equal format "rgb")
                                                3)
                                               ((not (string-equal palette-id id))
                                                1)
                                               (t
                                                (pixel-find-stride array comma))))))
                              (cond ((and (eq c 4) (string-equal format "nil"))
                                     (progn (setq format "rgba") c))
                                    ((and (eq c 3) (string-equal format "nil"))
                                     (progn (setq format "rgb") c))
                                    ((and (eq c 1) (string-equal format "nil"))
                                     (progn (setq format "palette") c))
                                    (t c)))))
             (w (or (read (or (match-string-no-properties 5) "nil"))
                    (/ (pixel-find-width array comma) stride)))
             (h (or (read (or (match-string-no-properties 6) "nil"))
                    (pixel-find-height array)))
             (open (print (match-string-no-properties 8)))
             (numbers (read (concat "(" (save-match-data (replace-regexp-in-string "[^0-9\\.]" " " array)) ")")))
             (type (or (match-string-no-properties 4)
                       (pixel-find-type numbers)))
             (colors (let ((colors)) (progn
                                       (dotimes (i (* h w) colors)
                                         (let ((r (nth (+ (* i stride) 0) numbers))
                                               (g (nth (+ (* i stride) 1) numbers))
                                               (b (nth (+ (* i stride) 2) numbers)))
                                           (add-to-list 'colors (apply 'color-rgb-to-hex (pixel-normalize-color type (list r g b))) t)
                                           ;; (setq colors (append colors (list (color-rgb-to-hex r g b))))
                                           ))))))
        (list :palette (list :id id
                             :format format
                             :type type
                             :open open
                             :comma comma
                             :colors colors
                             :close (cond ((string-equal open "(")
                                           ")")
                                          ((string-equal open "[")
                                           "]")
                                          ((string-equal open "{")
                                           "}")))
              :palette-origin (list :id
                                    :beginning (match-beginning 0)
                                    :end (match-end 0)
                                    :array-beginning (match-beginning 9)
                                    :array-end (match-end 9)
                                    :buffer (current-buffer)))))))

(defun pixel-origin-p (origin)
  (when (and (listp origin)
             (plist-get origin :beginning)
             (plist-get origin :end)
             (plist-get origin :buffer))
    origin))

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

(defun* pixel-find-palette (&key (id nil) (bitmap nil) (point nil) (origin nil) (find-origin nil))
  (cond ((numberp point)
         (plist-get (pixel-read-palette-at-point point) (if find-origin :palette-origin :palette)))
        ((pixel-origin-p origin)
         (plist-get (pixel-read-palette-at-point (plist-get origin :beginning)) (if find-origin :palette-origin :palette)))
        ((or (stringp id)
             (and (pixel-bitmap-p bitmap) (setq id (plist-get bitmap :palette-id))))
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

;; (pixel-find-palette :bitmap (pixel-find-bitmap :id "test1"))

(defun* pixel-find-bitmap (&key (id nil) (point nil) (origin nil) (find-origin nil))
  (cond ((numberp point)
         (plist-get (pixel-read-bitmap-at-point point) (if find-origin :bitmap-origin :bitmap)))
        ((pixel-origin-p origin)
         (plist-get (pixel-read-bitmap-at-point (plist-get origin :beginning)) (if find-origin :bitmap-origin :bitmap)))
        ((stringp id)
         (save-excursion
           (goto-char (point-min))
           (let ((case-fold-search nil))
             (if (re-search-forward (pixel-regex :bitmap t :mm major-mode :id id) nil t)
                 (plist-get (pixel-read-bitmap-at-point) (if find-origin :bitmap-origin :bitmap))
               (let ((buffers (pixel-list-buffer))
                     (result nil))
                 (while (and buffers
                             (not (with-current-buffer (pop buffers)
                                    (save-excursion
                                      (goto-char (point-min))
                                      (when (re-search-forward (pixel-regex :bitmap t :mm major-mode :id id) nil t)
                                        (setq result (plist-get (pixel-read-bitmap-at-point) (if find-origin :bitmap-origin :bitmap)))))))))
                 result)))))))

(defun* pixel-list-editor (&key (buffer nil))
  (let ((buffers (if buffer `(,buffer) (pixel-list-buffer)))
        (result '()))
    (loop for buf in buffers
          do (with-current-buffer buf
               (save-excursion
                 (goto-char (point-min))
                 (let ((case-fold-search nil))
                   (while (re-search-forward (pixel-regex :bitmap t :mm major-mode :quick t) nil t)
                     (let ((overlays (overlays-at (point))))
                       (dolist (ov overlays)
                         (let ((editor (overlay-get ov 'pixel-editor)))
                           (when editor
                             (add-to-list 'result editor))))))))))
    result))

(defun* pixel-list-bitmap (&key (buffer nil) (list-origin nil))
  (let ((buffers (if buffer `(,buffer) (pixel-list-buffer)))
        (result '()))
    (loop for buf in buffers
          do (with-current-buffer buf
               (save-excursion
                 (goto-char (point-min))
                 (let ((case-fold-search nil))
                   (while (re-search-forward (pixel-regex :bitmap t :mm major-mode) nil t)
                     (let ((id (match-string 1)))
                       (add-to-list 'result (if list-origin
                                                (plist-get (pixel-read-bitmap-at-point) :bitmap-origin)
                                              (pixel-cached (lambda (&rest args) (plist-get (pixel-read-bitmap-at-point) :bitmap))
                                                            'pixel-bitmap-cache
                                                            :id id))))
                     ;;(add-to-list 'result (plist-get (pixel-read-bitmap-at-point) (if list-origin :bitmap-origin :bitmap)))
                     )))))
    result))

(defun* pixel-find-editor (&key (id nil) (point nil) (origin nil) (find-origin nil))
  (let ((ovs (cond ((numberp point)
                    (overlays-at point))
                   ((pixel-origin-p origin)
                    (overlays-at (plist-get origin :beginning)))
                   ((stringp id)
                    (let ((origin (pixel-find-bitmap :find-origin t :id id)))
                      (when (pixel-origin-p origin)
                        (with-current-buffer (plist-get origin :buffer)
                            (overlays-at (plist-get origin :beginning))))))
                   (t
                    (overlays-at (point)))))
        (editor nil))
    (while (and ovs (not (setq editor (overlay-get (pop ovs) 'pixel-editor)))))
    editor))

(defun pixel-cached (fn cache &rest args)
  (let ((key (or (plist-get args :id)
                 (plist-get (plist-get args :origin) :id)
                 (plist-get (plist-get args :bitmap) :palette-id))))
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

(defun* pixel-toggle-editor (&key (id nil) (point nil) (origin nil) (remove-active t))
  (interactive)
  (unless (pixel-origin-p origin)
    (setq origin (cond ((stringp id)
                        (pixel-find-bitmap :id id :find-origin t))
                       ((numberp point)
                        (pixel-find-bitmap :point point :find-origin t))
                       (pixel-mode
                        (pixel-find-bitmap :point (point) :find-origin t)))))
  (let ((editor (pixel-find-editor :origin origin))
        (modified-state (buffer-modified-p)))
    (when (and editor remove-active)
      (pixel-editor-remove editor))
    (when (not editor)
      (let* ((bitmap (pixel-cached 'pixel-find-bitmap 'pixel-bitmap-cache :origin origin))
             (palette (pixel-cached 'pixel-find-palette 'pixel-palette-cache :bitmap bitmap))
             (editor (pixel-editor-create bitmap palette origin
                                          :background "#2f2f2f"
                                          :foreground "#ffffff"
                                          :source-background "#222222")))
        (pixel-editor-insert-tools editor 'pixel 'fill)
        (pixel-editor-insert-canvas editor palette bitmap)
        (pixel-editor-insert-palette editor palette)
        (set-buffer-modified-p modified-state)))))

(defvar pixel-restore-editor-after-save-list '())

(defvar pixel-global-bitmap-cache (make-hash-table :test 'equal))
(defvar pixel-global-palette-cache (make-hash-table :test 'equal))
(defvar pixel-global-buffer-cache (make-hash-table :test 'equal))

(defun pixel-before-save ()
  (let ((pixel-bitmap-cache pixel-global-bitmap-cache)
        (pixel-palette-cache pixel-global-palette-cache)
        (pixel-buffer-cache pixel-global-buffer-cache))
    (dolist (editor (pixel-list-editor :buffer (current-buffer)))
      ;;(pixel-editor-save editor)
      (pixel-editor-remove editor)
      (add-to-list 'pixel-restore-editor-after-save-list (plist-get editor :id)))))

(defun pixel-after-save ()
  (let ((pixel-bitmap-cache pixel-global-bitmap-cache)
        (pixel-palette-cache pixel-global-palette-cache)
        (pixel-buffer-cache pixel-global-buffer-cache))
    (dolist (id pixel-restore-editor-after-save-list)
      (pixel-toggle-editor :id id)))
  (setq pixel-restore-editor-after-save-list '())
  (clrhash pixel-global-bitmap-cache)
  (clrhash pixel-global-palette-cache)
  (clrhash pixel-global-buffer-cache))

(defadvice save-buffer (around pixel-save-buffer-advice last activate)
  (let ((editor-list (pixel-list-editor :buffer (current-buffer))))
    (if (and (buffer-modified-p)
             editor-list)
        (let ((buffer (get-buffer-create (concat "copy of " (buffer-name)))))
          (copy-to-buffer buffer (point-min) (point-max))
          (dolist (editor editor-list)
            (with-current-buffer buffer
              (let ((ov (plist-get editor :ov-editor)))
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
        (mapc 'pixel-editor-remove (pixel-list-editor :buffer (current-buffer)))
        (remove-hook 'before-save-hook 'pixel-before-save)
        (remove-hook 'after-save-hook 'pixel-after-save))
    (let ((pixel-bitmap-cache (make-hash-table :test 'equal))
          (pixel-palette-cache (make-hash-table :test 'equal))
          (pixel-buffer-cache (make-hash-table :test 'equal)))
      (add-hook 'before-save-hook 'pixel-before-save)
      (add-hook 'after-save-hook 'pixel-after-save)
      (dolist (origin (pixel-list-bitmap :buffer (current-buffer) :list-origin t))
        (pixel-toggle-editor :origin origin)))))
