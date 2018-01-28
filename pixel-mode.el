;;; pixel-mode.el --- Minor mode for drawing pixel art

;; Copyright (C) 2014-2017 Andreas Raster

;; Author: Andreas Raster <lazor@affenbande.org>
;; Version: 0
;; Keywords: games, multimedia, convenience, pixelart, drawing, pixel, art
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/rakete/pixel-mode

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

;; A minor mode for drawing pixel art inside emacs buffers. This makes
;; it possible to easily create programming language arrays representing
;; bitmaps visually. They can be drawn with the mouse right inside an
;; emacs buffer, making this mode a very convenient tool to quickly
;; create icons or other small 'programmer art'.

;; This file contains the code to deal with matching arrays as bitmaps
;; or palettes and reading them into the appropriate data structures,
;; plus various ways to verify regular expression matches and deduce
;; information from them.
;; It contains the code that deals with managing editors inside buffers,
;; enabling, disabling and finding them. Associating the right palette
;; with the right bitmap.
;; And saving buffers while pixel editors are active.

;;; Code:
(require 'cl)
(require 'color)

(require 'pixel-bitmap)
(require 'pixel-editor)

(defun* pixel-regex (&key (bitmap nil) (palette nil) (id nil) (quick nil) (mm nil))
  "Build regular expression for matching pixel-mode bitmaps.

Central part of pixel-mode is matching array definitions
in various programming languages, that represent either
bitmaps or palettes, with a regular expression. This
function creates such a regular expression, fitting the
programming major mode that is currently active.

The regular expression that is returned can be customized
with the following parameters to this function:

- BITMAP and PALETTE control whether you want to match an
  array representing a bitmap or a palette, if none is set
  this function assumes you want to match both, if either
  of BITMAP or PALETTE is set to t, it only matches the
  corresponding array. Additionally you can set palette to
  a string of a palette id to match only those bitmaps
  using that palette id.
- ID can be a specific id of either a palette or bitmap,
  to match only those that have that id
- QUICK produces a faster to match regular expression in
  case you only want to match the header of a bitmap or
  palette and don't care about the whole array
- MM can be used to force this function to assume a specific
  major-mode

The returned regular expression contains several groups
that can be used to extract specific information about
the matched bitmap or palette. These are as follows:

1. Should be the id of the matched bitmap or palette.
2. If the matched array is a bitmap, this should be the
   id of the palette this bitmap uses, or nothing otherwise.
   This is an optional group.
3. Format of the image tuples either \"rgb\" or \"rgba\", if
   the bitmap uses a palette this can be ignored. This is an
   optional group.
4. The type of the image tuples, this may be something like
   \"int\" or \"float\". This is an optional group.
5. The width of the bitmap. This is an optional group.
6. The height of the bitmap. This is an optional group.
7. The number of components per pixel. This is an optional
   group.
8. The opening parenthesis before the array. Like \"(\" or
   \"{\". This is an optional group.
9. The actual array containing the bitmap pixel, or palette
   colors as either indices into an palette, or as color
   tuples.

Here is an example that demonstrates what a regular
expression produced by this function can match. These are
two C arrays representing a palette and a bitmap.

First the array representing a palette:

// palette: default_font_palette
// format: rgb
uint8_t global_default_font_palette[1*9*3] = {
    0,   0,   0,
    255, 255, 255,
    0,   0,   0,
    255,   0,   0,
    0, 255,   0,
    0,   0, 255,
    255, 255,   0,
    0, 255, 255,
    230,   0, 150
};

Notice the first two commented lines, which identify this
array as a palette with id \"default_font_palette\" with the
format \"rgb\" for the color tuples. Since this is a C array
there is a type identifier \"uint8_t\" and the numbers in
the array brackets \"[1*9*3]\" can be matched as width=1,
height=9 and components=3.

Second the array representing a bitmap:

// bitmap: char_A
// using: default_font_palette
static int32_t pixels[6*7] = {
    0, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 0, 0,
    0, 1, 0, 0, 1, 0,
    0, 1, 1, 1, 1, 0,
    0, 1, 0, 0, 1, 0,
    0, 1, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0
};

Again two commented lines identify this array as bitmap
with id \"char_A\" using the palette \"default_font_palette\".
There is a type \"int32_t\" to match and the brackets
\"[6*7]\" can be matched as width=6 and height=7.

The above is also an example of a bitmap that uses indices
into a palette as pixels. It is also possible to have a
bitmap use rgb tuples as shown below.

// bitmap: bitmap_palette
static uint32_t pixels[2*2] = {
    255, 0, 0, 0, 0, 255,
    0, 0, 255, 255, 0, 0
};

A bitmap like that acts as its own palette, and can also be
referenced by another bitmap in its \"using\" clause.
"
  (unless mm
    (setq mm major-mode))
  ;; both bitmap and palette have nil as default value so that setting either to true results
  ;; in the other still being nil, but if none of them is set, I actually want this function
  ;; to assume that both bitmap and palette should be matched, so I have to set both to true
  ;; manually in that case
  (unless (or palette bitmap)
    (setq palette t
          bitmap t))
  (let* ((pixel-types-regex "\\(?:u\\|GL\\)\\(int\\|float\\)\\(?:8_t\\|16_t\\|32_t\\|64_t\\|max_t\\|ptr_t\\|_least8_t\\|_least16_t\\|_least32_t\\|_least64_t\\|_fast8_t\\|_fast16_t\\|_fast32_t\\|_fast64_t\\)")
         (palette-id (or (and (stringp palette) palette) "[^\t\n]+"))
         (init (cond ((or (stringp palette) (and palette bitmap)) "\\(?:\\<palette\\>\\|\\<bitmap\\>\\)")
                     (palette "\\<palette\\>")
                     (bitmap "\\<bitmap\\>")))
         (palette-optional (and (or (not (stringp palette))
                                    (string-equal palette id))
                                "?"))
         (id (if id
                 (regexp-quote id)
               "[^ \t\n]+"))
         (quick-re (concat "^[ \t/\*;#%-]*" init ":[ \t]*\\(" id "\\)\n"
                           ;; comment   palette|bitmap   1:specific or any name
                           "\\(?:[ \t/\*;#%-]*using:[ \t]*\\(" palette-id "\\)" "[ \t\n]+" "\\)" palette-optional
                           ;; comment, 2:specific or any palette name when init=bitmap, whole thing may be optional
                           ;; a bitmap without a palette param can be itself a palette, when it has colors packed in (rgb triples)
                           "\\(?:[ \t/\*;#%-]*format:[ \t]*\\(" "[^ \t\n]+" "\\)" "[ \t\n]+" "\\)?"
                           ;; 3:format of the image (rgba or rgb, defaults to rgb)
                           ))
         (full-re (unless quick
                    (concat
                     (cond ((or (eq mm 'c-mode)
                                (eq mm 'c++-mode)
                                (eq mm 'js2-mode))
                            (concat "\\(?:""\\(?:[^\[\(\{\"]*" pixel-types-regex "[ \t]+\\)?[^\[]+\\[\\([0-9]+\\)\\*\\([0-9]+\\)\\(?:\\*\\([0-9]+\\)\\)?\\]" "\\)?"))
                           ((or (eq mm 'emacs-lisp-mode)
                                (eq mm 'lisp-interaction-mode)
                                (eq mm 'lisp-mode)
                                (eq mm 'scheme-mode)
                                (eq mm 'hy-mode))
                            "\\(?:[^ \t\n]*\\(?:[ \t\n]+\\(?:\\(?:[:']type[: ]*[\"']*\\([a-zA-Z]+\\)[\"']*\\)\\|\\(?:[:']width[: ]*[\"']*\\([0-9]+\\)[\"']*\\)\\|\\(?:[:']height[: ]*[\"']*\\([0-9]+\\)[\"']*\\)\\|\\(?:[:']stride[: ]*[\"']*\\([0-9]+\\)[\"']*\\)\\)\\)\\{0,4\\}\\)")
                           (t
                            "\\(\\)\\(\\)\\(\\)\\(\\)"))
                     ;; search for 4:types, if any, then search for c-style array indicator, getting 5:width, 6:height and optionally 7:number of components
                     "\\(?:.*\n\\)*?.*\\(\\[\\|\(\\|\{\\)" "[ \t\n]*?"
                     ;; 8:open
                     "\\(\\(?:[ \t/\*;#%-]*\\(?:[0-9\\.]+\\|[0-9]+\\)\\(?:[ \t,f]*\\)?\n?\\)+\\)"
                     ;; 9:array
                     ))))
    (concat quick-re full-re)))

(defun* pixel-gimp-palette-regex (&key (id nil))
  "A helper function that creates a regex for matching a GIMP palette.
This works just like `pixel-regex', but only matches GIMP palettes."
  (let* ((palette-id (or (and (stringp id) id) "[^\t\n]+")))
    (concat "^GIMP Palette[ \t\n]*"
            "Name:[ \t]*\\(" palette-id "\\)[ \t\n]*"
            "Columns:[ \t]*[^\t\n]*[ \t\n]*"
            "#[ \t\n]*"
            "\\(þ\\)?\\(þ\\)?\\(þ\\)?\\(þ\\)?\\(þ\\)?\\(þ\\)?\\(þ\\)?"
            "\\(\\(?:[ \t]*[0-9]+[ \t]*[0-9]+[ \t]*[0-9]+[ \t]*[^\n]*\n*\\)+\\)")))

(defun* pixel-print-match (&key (bitmap nil) (palette nil) (id nil) (quick nil) (mm nil))
  "After matching a `pixel-regex', this just prints all group matches."
  (interactive)
  (when (thing-at-point-looking-at (print (pixel-regex :bitmap bitmap :palette palette :id id :quick quick :mm mm)))
    (message "%s %s %s %s %s %s %s %s\n%s"
             (match-string 1)
             (match-string 2)
             (match-string 3)
             (match-string 4)
             (match-string 5)
             (match-string 6)
             (match-string 7)
             (match-string 8)
             (match-string 9))))

(defun* pixel-verify-match (&key (bitmap nil) (palette nil) (id nil) (match nil))
  "Try to verify if the last regular expression match has correctly matched a pixel-regex bitmap or palette."
  (and (match-string 0 match)
       (> (length (match-string 0 match)) 0)
       (if (stringp id) (string-equal (match-string 1 match) id) t)))

(defun pixel-mapx (x f xs)
  "Given a list XS, apply function F to the index and X elements from the list.
So for example (pixel-mapx 3 f '(a b c)) calls function f like this (f 0 a b c)
once.

I also like to call X the stride."
  (when (< x 1)
    (setq x 1))
  (let ((steps (ceiling (/ (float (length xs)) (float x)))))
    (loop for n from 0 below steps
          collect (apply f n (subseq xs (* n x) (+ (* n x) x))))))

;; (pixel-mapx 4 (lambda (index &rest args) (print (format "%d: %S" index args))) '(1 2 3 4 5 6 7 8))

(defun pixel-find-comma (source)
  "This tries to determine the comma character from a SOURCE text. A source text is just
the portion of a bitmap or palette definition that defines the actual bitmap or palette,
so the array that has the pixels in it. So in something like this:

int foo[3] = {
    1, 2, 3,
    4, 5, 6,
    7, 8, 9
};

The 1,2,3,4,5,6,7,8,9 part is what you would pass as SOURCE text to this function.

Then this function looks if there is a comma or nothing between the digits of the
source text with a regex."
  (save-match-data
    (string-match "^[^0-9\\.]*[0-9\\.]+\\([^0-9\\.]+\\)[0-9\\.]+.*" source)
    (condition-case nil (match-string 1 source) (error ","))))

;; (pixel-find-comma ";; { 0, 0, 0, 0, 0, 0, 0")

(defun pixel-find-width (source &optional comma)
  "This tries to determine the bitmap width of a SOURCE text by counting the amount of commas
in the first line.

Can be forced to used a certain string as comma with an optional COMMA argument.

See also `pixel-find-comma' for details on what a source text is."
  (save-match-data
    (let ((comma-re (concat "[" (regexp-quote (remove-duplicates (or comma (pixel-find-comma source)))) "]")))
      (string-match (concat "^[^0-9\\.]*\\(\\(?:[0-9\\.]+" comma-re "*\\)+\\)") source)
      (length (split-string (match-string 1 source) comma-re t)))))

;; (pixel-find-width ";; 0 , 0 , 0 , 0 , 0 ,  |0 0 0 0 0")

(defun pixel-find-height (source)
  "Determine height of bitmap by counting lines in SOURCE text.

See also `pixel-find-comma' for details on what a source text is."
  (save-match-data
    (length (split-string source "\n" t))))

;; (pixel-find-height ";; 0 , 0 , 0 , 0 , 0 ,  \n0 0 0 0 0")

(defun pixel-find-type (numbers)
  "Given some NUMBERS, determine if those are floats or ints. Even if just one number is
a float, this will return float, otherwise it returns int."
  (if (some 'floatp numbers)
      "float"
    "int"))

;; (pixel-find-type '(0 0 0 0 0.1))

(defun pixel-read-bitmap (&optional marker)
  "This function uses the groups from `pixel-regex' to read a bitmap into a plist. It can
be applied to specific region of a buffer by supplying a MARKER, otherwise it will just
assume that there is a bitmap to match at the current point.

It returns two plists, one for the bitmap and one describing the origin where the bitmap
was matched, like the which buffer, location in the buffer, etc.

See also `pixel-read-palette'."
  (interactive)
  (with-current-buffer (or (when (markerp marker) (marker-buffer marker)) (current-buffer))
    (save-excursion
      (when (markerp marker)
        (goto-char (marker-position marker)))
      (when (thing-at-point-looking-at (pixel-regex :bitmap t))
        (let* ((id (match-string-no-properties 1))
               (palette-id (or (match-string-no-properties 2) id))
               (format (or (match-string-no-properties 3) "nil"))
               (array (match-string-no-properties 9))
               (comma (pixel-find-comma array))
               (stride (let ((c (or (read (or (match-string-no-properties 7) "nil"))
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
                               (t c))))
               (w (or (read (or (match-string-no-properties 5) "nil"))
                      (/ (pixel-find-width array comma) stride)))
               (h (or (read (or (match-string-no-properties 6) "nil"))
                      (pixel-find-height array)))
               (open (match-string-no-properties 8))
               (numbers (read (concat "(" (save-match-data (replace-regexp-in-string "[^0-9\\.]" " " array)) ")")))
               (type (or (match-string-no-properties 4)
                         (pixel-find-type numbers)))
               (d (- (length numbers) (* w h stride)))
               (colors (cond ((string-equal id palette-id)
                              (plist-get (plist-get (pixel-read-palette marker) :palette) :colors))
                             ((or (string-equal format "rgb")
                                  (string-equal format "rgba"))
                              (save-match-data
                                (plist-get (pixel-find-palette :id palette-id) :colors)))))
               (alpha (make-vector (* w h) 1))
               (array (apply 'vector (pixel-mapx stride (lambda (n &rest color)
                                                          (when (eq (length color) 4)
                                                            (setf (elt alpha n) (nth 3 color)))
                                                          (if (or (string-equal id palette-id)
                                                                  (string-equal format "rgb")
                                                                  (string-equal format "rgba"))
                                                              (let* ((color-name (cond ((eq (length color) 3)
                                                                                         (apply 'color-rgb-to-hex (pixel-normalize-color type color)))
                                                                                        ((eq (length color) 3)
                                                                                         (apply 'color-rgb-to-hex (butlast (pixel-normalize-color type color) 1)))))
                                                                     (pos (position color-name colors :test 'equal)))
                                                                (or pos (position (pixel-palette-similar-color (list :colors colors) color-name) colors :test 'equal)))
                                                            (car color)))
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
                                     :buffer (current-buffer))))))))

(defun pixel-find-stride (source &optional comma)
  "Given a SOURCE text, determine the stride to use with `pixel-mapx'.

See also `pixel-find-comma' for details on what a source text is."
  (let ((w (pixel-find-width source comma)))
    (cond ((eq (mod w 3) 2)
           4)
          ((eq (mod w 3) 1)
           nil)
          (t
           3))))

(defun pixel-read-palette (&optional marker)
  "Same as `pixel-read-bitmap' but this reads a palette instead."
  (interactive)
  (with-current-buffer (or (when (markerp marker) (marker-buffer marker)) (current-buffer))
    (save-excursion
      (when (markerp marker)
        (goto-char (marker-position marker)))
      (when (or (thing-at-point-looking-at (pixel-regex))
                (thing-at-point-looking-at (pixel-gimp-palette-regex)))
        (let* ((id (match-string-no-properties 1))
               (palette-id (match-string-no-properties 2))
               (format (or (match-string-no-properties 3) "nil"))
               (array (match-string-no-properties 9))
               (comma (pixel-find-comma array))
               (stride (let ((c (or (read (or (match-string-no-properties 7) "nil"))
                                    (cond ((string-equal format "rgba")
                                           4)
                                          ((string-equal format "rgb")
                                           3)
                                          ((and (stringp palette-id)
                                                (not (string-equal palette-id id)))
                                           1)
                                          (t
                                           (pixel-find-stride array comma))))))
                         (cond ((and (eq c 4) (string-equal format "nil"))
                                (progn (setq format "rgba") c))
                               ((and (eq c 3) (string-equal format "nil"))
                                (progn (setq format "rgb") c))
                               ((and (eq c 1) (string-equal format "nil"))
                                (progn (setq format "palette") c))
                               (t c))))
               (w (or (read (or (match-string-no-properties 5) "nil"))
                      (/ (pixel-find-width array comma) stride)))
               (h (or (read (or (match-string-no-properties 6) "nil"))
                      (pixel-find-height array)))
               (open (match-string-no-properties 8))
               (numbers (read (concat "(" (save-match-data (replace-regexp-in-string "[^0-9\\.]+" " " array)) ")")))
               (type (or (match-string-no-properties 4)
                         (pixel-find-type numbers)))
               (colors (let ((colors))
                         (progn
                           (dotimes (i (* h w) colors)
                             (let ((r (nth (+ (* i stride) 0) numbers))
                                   (g (nth (+ (* i stride) 1) numbers))
                                   (b (nth (+ (* i stride) 2) numbers)))
                               (add-to-list 'colors
                                            (apply 'color-rgb-to-hex (pixel-normalize-color type (list r g b)))
                                            t
                                            ;; why am I setting the compare-fn to nil if palette-id is stringp?
                                            ;; 'equal makes duplicate colors go away
                                            (if (stringp palette-id)
                                                (lambda (a b) nil)
                                              'equal))
                               ;; (setq colors (append colors (list (color-rgb-to-hex r g b))))
                               )))))
               (symbols (loop for i from 0 upto (length colors) collect (prin1-to-string i))))
          (list :palette (list :id id
                               :colors colors
                               :symbols symbols
                               :format format
                               :type type
                               :open open
                               :comma comma
                               :close (cond ((string-equal open "(")
                                             ")")
                                            ((string-equal open "[")
                                             "]")
                                            ((string-equal open "{")
                                             "}")))
                :palette-origin (list :id id
                                      :beginning (match-beginning 0)
                                      :end (match-end 0)
                                      :array-beginning (match-beginning 9)
                                      :array-end (match-end 9)
                                      :buffer (current-buffer))))))))

(defun pixel-origin-p (origin)
  "Test if a plist ORIGIN is a origin as returned by either `pixel-read-bitmap' or `pixel-read-palette'."
  (when (and (listp origin)
             (plist-get origin :id)
             (plist-get origin :beginning)
             (plist-get origin :end)
             (plist-get origin :buffer))
    origin))

(defun pixel-origin-marker (origin)
  "Given ORIGIN make a marker for it."
  (set-marker (make-marker) (plist-get origin :beginning) (plist-get origin :buffer)))

(defun* pixel-list-buffer ()
  "List all buffers that contain a bitmap or palette that can be matched with `pixel-regex'."
  (pixel-cached (lambda (&rest args)
                  (let ((result '()))
                    (loop for buf in (buffer-list)
                          do (with-current-buffer buf
                               (when (and (not buffer-read-only)
                                          (buffer-file-name (current-buffer))
                                          (save-excursion
                                            (goto-char (point-min))
                                            (let ((case-fold-search nil))
                                              (re-search-forward (pixel-regex :quick t) nil t))))
                                 (add-to-list 'result buf))))
                    result))
                'pixel-buffer-cache
                :id "pixel-buffers"))

(defvar pixel-palettes-directory
  (concat (file-name-directory (or load-file-name buffer-file-name)) "palettes")
  "Directory in which `pixel-mode' searches for palette files.")

(defun* pixel-find-palette (&key (id nil) (bitmap nil) (marker nil) (origin nil) (find-origin nil))
  "Find specific palette. Given an ID find a matching palette, given an BITMAP find the
palette that matches the bitmaps palette id, given a MARKER find a palette at that
 arker, given an ORIGIN find a palette at a marker created from that origin.

Set FIND-ORIGIN to make this function return the origin instead of the palette.

This will also search `pixel-palettes-directory' for matching palettes.

Uses `pixel-list-buffer' to search a list of buffers for palettes."
  (when (and (pixel-bitmap-p bitmap)
             (setq id (plist-get bitmap :palette-id))))
  (let* ((id-regex (when id (concat "^" (replace-regexp-in-string "[ \t\n]" "." (regexp-quote id)))))
         (id-matches nil))
    (cond ((and (file-exists-p pixel-palettes-directory)
                (setq id-matches (directory-files pixel-palettes-directory nil id-regex))
                (file-exists-p (concat pixel-palettes-directory "/" (car-safe id-matches))))
           (with-current-buffer (find-file-noselect (file-truename (concat pixel-palettes-directory "/" (car-safe id-matches))))
             (save-excursion
               (goto-char (point-min))
               (plist-get  (pixel-read-palette) (if find-origin :palette-origin :palette)))))
          ((markerp marker)
           (plist-get (pixel-read-palette marker) (if find-origin :palette-origin :palette)))
          ((pixel-origin-p origin)
           (plist-get (pixel-read-palette (pixel-origin-marker origin)) (if find-origin :palette-origin :palette)))
          ((stringp id)
           (save-excursion
             (goto-char (point-min))
             (let ((case-fold-search nil))
               (if (and (re-search-forward (pixel-regex :palette id :id id) nil t)
                        (pixel-verify-match :id id))
                   (plist-get (pixel-read-palette) (if find-origin :palette-origin :palette))
                 (let ((buffers (pixel-list-buffer))
                       (result nil))
                   (while (and buffers (not result))
                     (with-current-buffer (pop buffers)
                       (save-excursion
                         (goto-char (point-min))
                         (when (and (re-search-forward (pixel-regex :palette id :id id) nil t)
                                    (pixel-verify-match :id id))
                           (setq result (plist-get (pixel-read-palette) (if find-origin :palette-origin :palette)))))))
                   result))))))))

;; (pixel-find-palette :bitmap (pixel-find-bitmap :id "test1"))

(defun* pixel-find-bitmap (&key (id nil) (marker nil) (origin nil) (find-origin nil))
  "Find specific bitmap. Like `pixel-find-palette' but for bitmaps."
  (cond ((markerp marker)
         (plist-get (pixel-read-bitmap marker) (if find-origin :bitmap-origin :bitmap)))
        ((pixel-origin-p origin)
         (plist-get (pixel-read-bitmap (pixel-origin-marker origin)) (if find-origin :bitmap-origin :bitmap)))
        ((stringp id)
         (save-excursion
           (goto-char (point-min))
           (let ((case-fold-search nil))
             (if (and (re-search-forward (pixel-regex :bitmap t :id id) nil t)
                      (pixel-verify-match :id id))
                 (plist-get (pixel-read-bitmap) (if find-origin :bitmap-origin :bitmap))
               (let ((buffers (pixel-list-buffer))
                     (result nil))
                 (while (and buffers (not result))
                   (with-current-buffer (pop buffers)
                     (save-excursion
                       (goto-char (point-min))
                       (when (and (re-search-forward (pixel-regex :bitmap t :id id) nil t)
                                  (pixel-verify-match :id id))
                         (setq result (plist-get (pixel-read-bitmap) (if find-origin :bitmap-origin :bitmap)))))))
                 result)))))))

(defun* pixel-list-editor (&key (buffer nil))
  "List all active editors, use BUFFER argument to force searching through one single buffer."
  (let ((buffers (if buffer `(,buffer) (pixel-list-buffer)))
        (result '()))
    (loop for buf in buffers
          do (with-current-buffer buf
               (save-excursion
                 (goto-char (point-min))
                 (let ((case-fold-search nil))
                   (condition-case nil
                       (while (re-search-forward (pixel-regex :bitmap t :quick t) nil t)
                         (let ((overlays (overlays-at (point))))
                           (dolist (ov overlays)
                             (let ((editor (overlay-get ov 'pixel-editor)))
                               (when editor
                                 (add-to-list 'result editor))))))
                     (error nil))))))
    result))

(defun* pixel-list-bitmap (&key (buffer nil) (list-origin nil))
  "List all bitmaps. Force with BUFFER to search single buffer, use LIST-ORIGIN to list bitmap origins instead."
  (let ((buffers (if buffer `(,buffer) (pixel-list-buffer)))
        (result '()))
    (loop for buf in buffers
          do (with-current-buffer buf
               (save-excursion
                 (goto-char (point-min))
                 (let ((case-fold-search nil))
                   (while (re-search-forward (pixel-regex :bitmap t) nil t)
                     (let ((id (match-string 1)))
                       (add-to-list 'result (if list-origin
                                                (plist-get (pixel-read-bitmap) :bitmap-origin)
                                              (pixel-cached (lambda (&rest args) (plist-get (pixel-read-bitmap) :bitmap))
                                                            'pixel-bitmap-cache
                                                            :id id))))
                     ;;(add-to-list 'result (plist-get (pixel-read-bitmap) (if list-origin :bitmap-origin :bitmap)))
                     )))))
    result))

(defun* pixel-find-editor (&key (id nil) (point nil) (origin nil) (find-origin nil))
  "Find specific editor. Like `pixel-find-palette' but for editors."
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
  "This takes a function FN and a symbol CACHE and then uses CACHE to store the result
of FN or return a previously stored result.

To do that it assumes that ARGS is a plist with an :id, :origin or :bitmap that it can
use as key. It then treats CACHE like a hash-table and either returns the value associated
with the key in the hash-table, or stores the result of FN in the hash-table if there
nothing yet associated with the key in the hash-table."
  (let ((key (or (plist-get args :id)
                 (plist-get (plist-get args :origin) :id)
                 (plist-get (plist-get args :bitmap) :palette-id))))
    (or (when (and key (boundp cache) (hash-table-p (eval cache)))
          (or (gethash key (eval cache))
              (puthash key (apply fn args) (eval cache))))
        (apply fn args))))

(defun* pixel-toggle-editor (&key (id nil) (marker nil) (origin nil) (remove-active t))
  "Toggle a pixel-mode editor between visible and invisible state.

See also `pixel-editor-create' and `pixel-editor-remove'."
  (interactive)
  (unless (pixel-origin-p origin)
    (setq origin (cond ((stringp id)
                        (pixel-find-bitmap :id id :find-origin t))
                       ((markerp marker)
                        (pixel-find-bitmap :marker marker :find-origin t))
                       ((some (lambda (m) (eq m 'pixel-mode)) minor-mode-list)
                        (pixel-find-bitmap :marker (set-marker (make-marker) (point)) :find-origin t)))))
  (let ((editor (pixel-find-editor :origin origin))
        (modified-state (buffer-modified-p)))
    (when (and editor remove-active)
      (pixel-editor-remove editor))
    (when (not editor)
      (let* ((bitmap (pixel-cached 'pixel-find-bitmap 'pixel-bitmap-cache :origin origin))
             (palette (pixel-cached 'pixel-find-palette 'pixel-palette-cache :bitmap bitmap))
             (editor (pixel-editor-create origin
                                          :background "#2f2f2f"
                                          :foreground "#ffffff"
                                          :source-background "#222222")))
        (pixel-editor-init editor bitmap palette)
        (pixel-editor-insert-palette editor palette)
        (pixel-editor-insert-toolbar editor)
        (pixel-editor-insert-canvas editor bitmap palette)
        (set-buffer-modified-p modified-state)
        editor))))

;; (defvar pixel-restore-editor-after-save-list '())

;; (defvar pixel-global-bitmap-cache (make-hash-table :test 'equal))
;; (defvar pixel-global-palette-cache (make-hash-table :test 'equal))
;; (defvar pixel-global-buffer-cache (make-hash-table :test 'equal))

;; (defun pixel-before-save ()
;;   (let ((pixel-bitmap-cache pixel-global-bitmap-cache)
;;         (pixel-palette-cache pixel-global-palette-cache)
;;         (pixel-buffer-cache pixel-global-buffer-cache))
;;     (dolist (editor (pixel-list-editor :buffer (current-buffer)))
;;       ;;(pixel-editor-save editor)
;;       (pixel-editor-remove editor)
;;       (add-to-list 'pixel-restore-editor-after-save-list (plist-get editor :id)))))

;; (defun pixel-after-save ()
;;   (when pixel-restore-editor-after-save-list
;;     (let ((pixel-bitmap-cache pixel-global-bitmap-cache)
;;           (pixel-palette-cache pixel-global-palette-cache)
;;           (pixel-buffer-cache pixel-global-buffer-cache))
;;       (dolist (id pixel-restore-editor-after-save-list)
;;         (pixel-toggle-editor :id id)))
;;     (setq pixel-restore-editor-after-save-list '())
;;     (clrhash pixel-global-bitmap-cache)
;;     (clrhash pixel-global-palette-cache)
;;     (clrhash pixel-global-buffer-cache)))

;; around pixel-save-buffer-advice last activate
(defun pixel-save-buffer (orig-fun &rest args)
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
              (apply orig-fun args)
              (let ((process (get-buffer-process (current-buffer))))
                (when process (delete-process process)))
              (kill-buffer))
            (set-visited-file-name visited-file t t)
            (clear-visited-file-modtime)
            (set-buffer-modified-p nil)))
      ;;(save-buffer)
      (apply orig-fun args))))

(with-eval-after-load "pixel-mode"
  (advice-add 'save-buffer :around #'pixel-save-buffer))

;;;###autoload
(define-minor-mode pixel-mode
  "Create pixel art right inside your programming buffers"
  :lighter " Pixel"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            ;;(define-key map (kbd "C-c C-c") 'pixel)
            map)
  (if (not pixel-mode)
      (progn
        (mapc 'pixel-editor-remove (pixel-list-editor :buffer (current-buffer)))
        ;;(remove-hook 'before-save-hook 'pixel-before-save)
        ;;(remove-hook 'after-save-hook 'pixel-after-save)
        )
    (let ((pixel-bitmap-cache (make-hash-table :test 'equal))
          (pixel-palette-cache (make-hash-table :test 'equal))
          (pixel-buffer-cache (make-hash-table :test 'equal)))
      ;;(add-hook 'before-save-hook 'pixel-before-save)
      ;;(add-hook 'after-save-hook 'pixel-after-save)
      (dolist (origin (pixel-list-bitmap :buffer (current-buffer) :list-origin t))
        (pixel-toggle-editor :origin origin)))))

(provide 'pixel-mode)

;;; pixel-mode.el ends here
