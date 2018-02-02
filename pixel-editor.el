;;; pixel-editor.el --- Minor mode for drawing pixel art -*- lexical-binding: t -*-

;; xxxLocal variables:
;; xxxbyte-compile-warnings: (not cl-functions)
;; xxxEnd:

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

;; This file contains the user interface code of pixel-mode. An editor
;; is what pixel-mode calls such an user interface: a gui displayed inside
;; of an emacs buffer that can be used like an image manipulation program
;; to modify/create pixel art.
;; Here you'll find functionality for creating and updating the editor
;; gui, for synchronizing the editor state with the array in the buffer
;; and for reacting to user input by mouse and keyboard. Editors are
;; implemented using overlays and rely heavily on the feature that emacs
;; can display overlay text in a buffer without it actually being inserted
;; into the file.

;; At the very least an editor should contain a canvas, which is a visual
;; representation of the array this editor is used to modify (in other words,
;; the canvas shows the picture that is edited). But it may also have a
;; palette to pick colors from, a toolbar which has buttons for editor
;; tools and shortcuts to colors/tools and a section which displays the
;; edited array as text. What gui elements an editor contains and in what
;; order those elements appear in the buffer is customizable by the user.

;; Input from the user is handled via keymaps that are only active as long
;; as the cursor is within the editor. These keymaps define actions that
;; change the editor state, for example when changing the active tool, or
;; resizing the canvas. They also define motions to handle mouse input so
;; that the user can use the mouse to draw lines and rectangles by dragging,
;; and they define keypresses as a way to draw with the keyboard instead
;; of using the mouse.

;;; Code:
(require 'pixel-bitmap nil 'noerror)

;;
;; Utilities
;;
(defun pixel-make-image (&rest keys)
  (find-image (list keys)))

(defvar pixel-hover-face-cache (make-hash-table :test 'equal)
  "Used for caching the faces that are used when the mouse hovers over a pixel.

See also `pixel-pixel-cache' and `pixel-make-hover-face'.")

(defun pixel-make-hover-face (id x y)
  "Make a face that is used when the mouse hovers over a pixel.

Use with the 'mouse-face property.

The ID is used for caching the created face in `pixel-hover-face-cache'."
  (let ((name (format "%s|%d|%d" id x y)))
    (or (gethash name pixel-hover-face-cache nil)
        (puthash name
                 (let* ((sym (intern name))
                        (face (make-face sym)))
                   (face-spec-reset-face face)
                   (set-face-attribute face nil :box (list :line-width 3 :color "#ffffff" :style nil))
                   face)
                 pixel-hover-face-cache))))

(defvar pixel-pixel-cache (make-hash-table :test 'equal)
  "Hashtable that caches the the xpm images which are used to diplay pixels.

See also `pixel-make-pixel'.")

(defun pixel-make-pixel (color width &optional height)
  "Create an unicolor image that can be used to represent a single pixel.

Use this with the 'display property.

COLOR specifies what color the pixel should have, WIDTH and optional HEIGHT
can be used to specify the dimensions (in real pixels) that the pixel image
is going to have.

See also `pixel-pixel-cache', `pixel-xpm-data', `pixel-make-bitmap' and
`find-image'."
  (let ((key (format "%d%d%s" width (or height width) color)))
    (append (list 'image) (cdr (or (gethash key pixel-pixel-cache nil)
                                   (puthash key (let ((template (pixel-xpm-data (pixel-make-bitmap :width width :height (or height width)))))
                                                  (pixel-make-image  :type 'xpm
                                                                     :data template
                                                                     :color-symbols `(("col0"  . ,color))
                                                                     :height (or height width)))
                                            pixel-pixel-cache))))))

;;
;; State
;;
(defvar pixel-editor-state (make-hash-table :test 'equal)
  "Hashtable that keep track of each editors state. The keys are editor :ids,
the values are plists containing things like the currently active tool or the
selected color of an editor, but also things like the overlays that the editor
consists of.

See also `pixel-editor-put', `pixel-editor-update' and `pixel-editor-get'.")

(defun pixel-editor-put (editor prop &optional val)
  "Put a property PROP with a value VAL into the state of EDITOR saved in
`pixel-editor-state'.

See also `pixel-editor-update', `pixel-editor-get' and `plist-put'."
  (or (and (plist-get editor prop)
           (plist-put editor prop val))
      (let* ((key (plist-get editor :id)))
        (when key
          (if prop
              (puthash key (plist-put (or (gethash key pixel-editor-state)
                                          (puthash key '() pixel-editor-state))
                                      prop val)
                       pixel-editor-state)
            (puthash key val pixel-editor-state))
          editor))))

;; (pixel-editor-put (list :id "aaa" :foo "lala") :ficken "bar")

(defun pixel-editor-update (editor prop val)
  "Update poperties PROP with new value VAL in state of EDITOR saved in
`pixel-editor-state'.

See also `pixel-editor-put', `pixel-editor-get' and `plist-put'."
  (let* ((ov (pixel-editor-get editor :ov-editor))
         (start (overlay-start ov))
         (end (overlay-end ov))
         (pos (1- start)))
    (while (setq pos (text-property-any (1+ pos) end 'pixel-property prop))
      (let ((update-fn (get-text-property pos 'pixel-update)))
        (when (functionp update-fn)
          (add-text-properties pos (1+ pos) (funcall update-fn val)))))))

(defun pixel-editor-get (editor &optional prop default)
  "Get poperties PROP value from the state of EDITOR save in `pixel-editor-state'.

If there is no value in the editor state yet, return DEFAULT.

See also `pixel-editor-update', `pixel-editor-put' and `plist-get'."
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
  "Return the buffer in which EDITOR is displayed.

See also `overlay-buffer'."
  (overlay-buffer (pixel-editor-get editor :ov-complete)))

(defun pixel-editor-insert-toolbar (editor)
  "Helper function to put a clickable toolbar on an already created EDITOR.

See also `pixel-editor-create', `pixel-editor-insert-palette' and `pixel-editor-insert-canvas'."
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
  "Helper function to put a clickable PALETTE on an already created EDITOR.

See also `pixel-editor-create', `pixel-editor-insert-toolbar' and `pixel-editor-insert-canvas'."
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
  "Helper function to put a drawable canvas on an already created EDITOR.

The inserted canvas will display BITMAP using PALETTE.

See also `pixel-editor-create', `pixel-editor-insert-palette' and `pixel-editor-insert-toolbar'."
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

(defvar pixel-editor-overlays
  '(:ov-complete :ov-source :ov-editor :ov-seperator2 :ov-palette :ov-seperator3 :ov-toolbar :ov-seperator4 :ov-canvas)
  "Customize the order and visibility of editor 'sections'.

Each editor consists of several overlays, one for the part that shows the source code,
one for the part that contains the canvas, etc. This variable holds keys that identify
the different sections and can be used to change the order in which the sections are
inserted into a buffer or to disable a section altogether.

The following keys are looked for specifically by `pixel-editor-create':
:ov-complete : encompasses the whole editor
:ov-source : contains only the source code of the edited image
:ov-editor : the editor with all its toolbars and the canvas

All other keys are not looked for, but for each key an overlay is created and put into
the editor state in `pixel-editor-state' as key value pair.

An example of what this variable might look like:
(:ov-complete :ov-source :ov-editor
 :ov-seperator2 :ov-palette :ov-seperator3 :ov-toolbar :ov-seperator4 :ov-canvas)")

(defun pixel-editor-remove (editor)
  "Remove a displayed EDITOR from its buffer.

See also `pixel-toggle-editor' and `delete-overlay'."
  (let ((modified-state (buffer-modified-p)))
    (dolist (key pixel-editor-overlays)
      (when (eq key :ov-editor)
        (delete-region (overlay-start (plist-get editor key))
                       (overlay-end (plist-get editor key))))
      (when (plist-get editor key)
        (delete-overlay (plist-get editor key))))
    (unless (cl-find :ov-source pixel-editor-overlays)
      (delete-overlay (plist-get editor :ov-source)))
    (delete-overlay (plist-get editor :ov-array))
    (pixel-editor-put editor nil)
    (set-buffer-modified-p modified-state)))

(defun pixel-editor-init (editor bitmap &optional palette)
  "Initialize an EDITOR state.

The EDITOR argument is a plist as created by `pixel-editor-create'.

This functions initializes several values in the EDITOR state with default
values and values taken from BITMAP, which essentially sets EDITOR to display
BITMAP in its canvas. The PALETTE argument is optional since BITMAP acts as
its own palette or contains a reference to a palette.

See also `pixel-editor-put' and `pixel-editor-state'."
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
  "Go through `pixel-editor-overlays' and create the overlays for an editor,
return a plist describing the created editor.

This does ONLY create the overlays. After this function, `pixel-editor-init'
should be called to initialize the editors state in `pixel-editor-state'. And
then the functions `pixel-editor-insert-palette', `pixel-editor-insert-toolbar'
and `pixel-editor-insert-canvas' should be called to 'fill' the empty overlays
this function created with the actual editor content.

The ORIGIN argument is an origin as returned by `pixel-read-bitmap' or
`pixel-read-palette', see `pixel-origin-p' for more information.

The BACKGROUND, FOREGROUND and SOURCE-BACKGROUND arguments can be used to
specify the colors that should be used for back- and foreground and the source
code background respectivly.
"
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
             (ov-array (unless (cl-find :ov-source pixel-editor-overlays)
                         (let ((ov (make-overlay array-beg array-end)))
                           (overlay-put ov 'invisible t)
                           ov)))
             ;; when :ov-source is not in pixel-editor-overlays, we create a overlay here and hide the text
             (ov-source (unless (cl-find :ov-source pixel-editor-overlays)
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
                 (when (eq last-pos (point-max))
                   (let ((modified-state (buffer-modified-p)))
                     (save-excursion
                       (goto-char last-pos)
                       (insert "\n")
                       (set-buffer-modified-p modified-state))))
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

;; (pixel-editor-create (pixel-find-palette :id "bnw") (pixel-find-bitmap :id "test1") (pixel-find-bitmap :find-origin t :id "test1")
;;                      :background "#2f2f2f"
;;                      :foreground "#ffffff"
;;                      :source-background "#222222")


;;
;; Source
;;

(defun pixel-source-color (editor color &optional alpha)
  "Convert COLOR, with optional ALPHA component, from emacs name representation as
accepted by `color-name-to-rgb' into the format that fits the source code from
which EDITOR was created."
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
  "Replace pixel at position X, Y in the source code of EDITOR with COLOR.

The COLOR argument is a list (x y color) which can be created by calling
`pixel-source-color'.

The arguments X, Y and COLOR can be lists of multiple coordinates and colors
so that many occurances of colors can be replaced in one batch."
  (let* ((width (pixel-editor-get editor :bitmap-width))
         (height (pixel-editor-get editor :bitmap-height))
         (stride (pixel-editor-get editor :bitmap-stride))
         (comma (pixel-editor-get editor :bitmap-comma))
         (xs (if (listp x) x (list x)))
         (ys (if (listp y) y (list y)))
         (colors (if (listp color) color (make-list (length xs) color)))
         (pixels (sort (cl-mapcar #'list xs ys colors)
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
  "Create neighbouring coordinates as list for the X, Y coordinate."
  (mapcar (lambda (shift) (list (+ x (nth 0 shift))
                                (+ y (nth 1 shift))))
          '((0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1))))

(defun pixel-tool-fill (input type editor x0 y0 color0 alpha0 &optional x1 y1 color1 alpha1 state)
  "Tool for filling an area of a bitmap with a single color.

See also `pixel-make-action', `pixel-make-canvas-motion' and `pixel-make-canvas-keypress'"
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
  "Tool for drawing pixels by dragging the mouse.

See also `pixel-make-action', `pixel-make-canvas-motion' and `pixel-make-canvas-keypress'"
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
  "Tool for drawing a rectangle by dragging the mouse.

See also `pixel-make-action', `pixel-make-canvas-motion' and `pixel-make-canvas-keypress'"
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
  "Tool for drawing a filled rectangle by dragging the mouse.

See also `pixel-make-action', `pixel-make-canvas-motion' and `pixel-make-canvas-keypress'"
  (pixel-tool-rectangle input type editor x0 y0 color0 alpha0 x1 y1 color1 alpha1 (plist-put state :fill t)))

(defun pixel-canvas-point (editor &optional x y)
  "Compute `point' for pixel at coordinate X, Y on the canvas of EDITOR.

See also `get-text-property'."
  (unless (and x y)
    (setq x (get-text-property (point) 'pixel-x)
          y (get-text-property (point) 'pixel-y)))
  (when (and x y)
    (let ((w (pixel-editor-get editor :bitmap-width))
          (h (pixel-editor-get editor :bitmap-height))
          (ov (pixel-editor-get editor :ov-canvas)))
      (+ (overlay-start ov) 1 (* y 2) (* y w) x))))

(defun pixel-canvas-color (editor &optional x y)
  "Get the color of the pixel at coordinate X, Y on the canvas of EDITOR.

X or Y are optional, if not specified the 'pixel-color at `point' is used.

See also `pixel-canvas-point'."
  (unless (and x y)
    (setq x (get-text-property (point) 'pixel-x)
          y (get-text-property (point) 'pixel-y)))
  (when (and x y)
    (get-text-property (pixel-canvas-point editor x y) 'pixel-color)))

(defun pixel-canvas-alpha (editor &optional x y)
  "Get the alpha component of the pixel at coordinate X, Y on the canvas of EDITOR.

X or Y are optional, if not specified the pixel at `point' is used.

See also `pixel-canvas-point'."
  (unless (and x y)
    (setq x (get-text-property (point) 'pixel-x)
          y (get-text-property (point) 'pixel-y)))
  (when (and x y)
    (get-text-property (pixel-canvas-point editor x y) 'pixel-alpha)))

(defun pixel-canvas-discard (state)
  "Discard :layer-overlays from STATE."
  (let ((layer-overlays (plist-get state :layer-overlays)))
    (dolist (ov layer-overlays)
      (delete-overlay ov))))

(defun pixel-canvas-merge (editor state)
  "Merge :updates from STATE into the canvas of EDITOR.

When `pixel-make-canvas-motion' or `pixel-make-canvas-keypress' apply a tool
to an editors canvas, the tool does not directly affect the canvas but rather
returns a list of updates. This function applies these :updates from STATE
to the canvas of EDITOR, and additionally deletes all :layer-overlays from
STATE because these are not neccessary to keep once the :updates are applied.

See also `pixel-canvas-layer'."
  (pixel-canvas-discard state)
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

(defun pixel-canvas-layer (editor state)
  "Make a layer for EDITOR from the :updates in STATE.

As explained in `pixel-canvas-merge', the tool functions return a list
of updates instead of modifying the canvas directly. This function
creates overlays that show these updates to the user, without them
being applied yet to the canvas. These temporary overlays only showing
one possible update to the canvas, are called a layer.

There are two properties that are added to STATE by this function:
:layer which is a vector that can hold an overlay for every pixel
of the canvas. And :layer-overlays which holds the same overlays,
but as list (I don't remember why I implemented it like this).

See also `pixel-canvas-discard', `pixel-make-canvas-motion' and
`pixel-make-canvas-keypress'."
  (let* ((w (pixel-editor-get editor :bitmap-width))
         (h (pixel-editor-get editor :bitmap-height))
         (updates (sort (cl-copy-list (plist-get state :updates))
                        (lambda (a b)
                          (if (eq (nth 1 a) (nth 1 b))
                              (< (nth 0 a) (nth 0 b))
                            (< (nth 1 a) (nth 1 b))))))
         (layer (or (and (eq (length (plist-get state :layer)) (* w h))
                         (plist-get state :layer))
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
  "Refresh the canvas of EDITOR."
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
  "Replace the bitmap displayed on the canvas of EDITOR with BITMAP."
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
  "Resize the canvas of EDITOR and the bitmap displayed on it
by adding ADD-WIDTH and ADD-HEIGHT in pixels to it.

When called interactively without arguments this will prompt
the user to enter a new size manually.

See also `pixel-bitmap-resize'."
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
    (goto-char (point-min))
    (forward-line (+ l add-height))
    (pixel-canvas-view editor p)))

(defun pixel-canvas-view (editor &optional p)
  "Center view on the canvas of EDITOR and optionally point P."
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
  "Switch the current tool in EDITOR to TOOL.

See also `pixel-make-editor-keymap'."
  (message "%s" (prin1-to-string tool))
  (pixel-editor-put editor :tool-current tool))

(defvar pixel-draggable-tools '(pixel-tool-rectangle pixel-tool-rectangle-filled))

(defun pixel-make-canvas-keypress (input type editor &optional tool)
  "Create and return a lambda function to handle keypress events while
`point' is inside the canvas of EDITOR.

The returned lambda is supposed to be used as a definition of a key
in `define-key'. It handles a keypress by passing INPUT TYPE EDITOR
as well as the x and y coordinate, color and alpha of the pixel at
`point' to the currently selected tool in EDITOR, or to optionally
specified TOOL.

A TOOL is a function that takes the following argument list:
input type editor x0 y0 color0 alpha0 &optional x1 y1 color1 alpha1 state

where the first seven arguments are neccessary for a single action
of the tool, and the optional arguments are used when the tool is
dragged over the canvas. Dragging a tool means repeatedly calling
it with with x0, y0, color0, alpha0 being from the pixel where the
tools action was first invoked, and x1, y1, color1, alpha1 from the
pixel where the tool was dragged to. Also the last state argument
is only relevant when dragging, when only a single action is perfomed
the tool function simply changes the current state and returns a new
state, but when repeatedly calling the tool function when dragging
it may need to keep some state for its execution, which is achieved
by passing the returned state into the next call again.

Dragging for keypress events is implemented differently then dragging
for mouse events in `pixel-make-canvas-motion'. When a tool is marked
as draggable by being listed in `pixel-draggable-tools', it is switched
to dragging state and either aborts and discards its changes, or finishes
and merges its changes into the canvas.

While a tool is dragged, the cursor keys up, down, left, right cause
the dragging to occur, return or space cause the tool to finish dragging
and merge its result into the canvas and escape or C-g cause the dragging
to abort and its changes to be discarded.

While dragging the changes of the tool are only shown temporarily,
this is done with a layer created by `pixel-canvas-layer'. When the
dragging is finished this layer is merged into the canvas with
`pixel-canvas-merge'. When the dragging is aborted the layer is discarded
with `pixel-canvas-discard'.

See also `with-local-quit'."
  (lambda (&optional pos)
    (interactive)
    (let* ((x (get-text-property (point) 'pixel-x))
           (y (get-text-property (point) 'pixel-y))
           (color (get-text-property (point) 'pixel-color))
           (alpha (get-text-property (point) 'pixel-alpha))
           (tool (or tool (pixel-editor-get editor :tool-current))))
      (cond ((cl-some (lambda (draggable) (eq tool draggable)) pixel-draggable-tools)
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
                                     (let ((offset (- (point) (point-at-bol))))
                                       (forward-line -1)
                                       (goto-char (+ (point-at-bol) offset))))
                                    ((eq event 'right)
                                     (right-char 1))
                                    ((eq event 'down)
                                     (let ((offset (- (point) (point-at-bol))))
                                       (forward-line 1)
                                       (goto-char (+ (point-at-bol) offset))))
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
                   (pixel-canvas-discard state)
                 (pixel-canvas-merge editor state))))
            (t
             (pixel-canvas-merge editor (funcall tool input type editor x y color alpha)))))))

(defun pixel-make-canvas-motion (input type editor &optional tool)
  "Create and return a lambda function to handle mouse motion events while
`point' is inside the canvas of EDITOR.

This works very similar to `pixel-make-canvas-keypress', but it can make use
of emacs function `track-mouse' to keep track of mouse dragging.

See also `with-local-quit'."
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
          (pixel-canvas-discard state)
        (pixel-canvas-merge editor state)))))

;; (let ((event nil))
;;   (while (not (eq event 'escape))
;;     (setq event (read-event))
;;     (print event)))

(defun pixel-editor-shortcut-get (editor n)
  "Set foreground color of EDITOR to the color that shortcut N is set to.

See also `pixel-make-action'."
  (let ((shortcuts (pixel-editor-get editor :palette-shortcuts)))
    (pixel-editor-foreground-set editor (nth n shortcuts))))

(defun pixel-editor-shortcut-set (editor n &optional color)
  "Set shortcut N in EDITOR to COLOR or the current 'pixel-color at `point'.

See also `pixel-make-action'."
  (let ((shortcuts (pixel-editor-get editor :palette-shortcuts))
        (color (or color (get-text-property (point) 'pixel-color))))
    (setf (nth n shortcuts) color)
    (pixel-editor-update editor :palette-shortcuts shortcuts)
    (pixel-editor-put editor :palette-shortcuts shortcuts)))

(defun pixel-editor-foreground-set (editor &optional color)
  "Set foreground color of EDITOR to COLOR or the current 'pixel-color at `point'.

See also `pixel-make-action'."
  (let ((color (or color (get-text-property (point) 'pixel-color))))
    (pixel-editor-update editor :palette-foreground color)
    (pixel-editor-put editor :palette-foreground color)))

(defun pixel-canvas-pick-color (editor)
  "Pick color at `point' in EDITOR.

See also `pixel-make-action' and `pixel-canvas-color'."
  (let ((color (pixel-canvas-color editor)))
    (when color
      (pixel-editor-update editor :palette-foreground color)
      (pixel-editor-put editor :palette-foreground color))))

(defun* pixel-make-action (editor action &rest args)
  "Create and return a lambda function to trigger a ACTION that is applied
to the EDITOR and ARGS.

An ACTION is something that changes the state of the editor, but not the
canvas. For example `pixel-editor-shortcut-get', `pixel-editor-shortcut-set',
`pixel-editor-foreground-set' and `pixel-editor-pick-color' are all
actions.

See also `pixel-make-mouse-action'."
  (lambda (&optional pos)
    (interactive)
    (apply action (append (list editor) args))))

(defun* pixel-make-mouse-action (editor action &rest args)
  "Special version of `pixel-make-action' for actions triggered by clicking
with the mouse.

See also `mouse-set-point'."
  (lambda (event)
    (interactive "e")
    (mouse-set-point event)
    (apply action (append (list editor) args))))


(defun pixel-make-editor-keymap (editor map)
  "Define keybindings that are useable everywhere in EDITOR in MAP."
  (suppress-keymap map)
  (define-key map (kbd "d") (pixel-make-action editor 'pixel-editor-tool-switch 'pixel-tool-draw))
  (define-key map (kbd "f") (pixel-make-action editor 'pixel-editor-tool-switch 'pixel-tool-fill))
  (define-key map (kbd "r") (pixel-make-action editor 'pixel-editor-tool-switch 'pixel-tool-rectangle))
  (define-key map (kbd "R") (pixel-make-action editor 'pixel-editor-tool-switch 'pixel-tool-rectangle-filled))
  (define-key map (kbd "p") (pixel-make-action editor 'pixel-canvas-pick-color))
  (define-key map (kbd "<tab>") (pixel-make-action editor 'pixel-canvas-view))
  map)

(defun pixel-make-canvas-keymap (editor map)
  "Define keybindings that are useable on the canvas of EDITOR in MAP."
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
  "Define keybindings that are useable on the palette of EDITOR in MAP."
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

;;; pixel-editor.el ends here
