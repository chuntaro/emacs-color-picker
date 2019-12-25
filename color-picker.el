;;; color-picker.el --- Color picker for mouse       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; URL: https://github.com/chuntaro/emacs-color-picker
;; Package-Requires: ((emacs "26.1") (posframe "20191219.57"))
;; Version: 0.1
;; Keywords: color, mouse, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This tool is a color picker that can be operated with a mouse for
;; the GUI version of Emacs.  Refer to the following extensions for
;; the Atom text editor.
;; A Color Picker for Atom
;; https://atom.io/packages/color-picker

;; Note: This tool works only on Windows version of Emacs!
;; So it's still pre-alpha.

;; Usage:
;; Add the following to your .emacs and run on the text that specifies
;; the color, such as `rgb(255, 0, 153)'.

;; (require 'color-picker)
;; (global-set-key [mouse-1] 'cpick-popup-at-point)
;; (global-set-key "\C-cp" 'cpick-popup-at-point)

;; Press `q' to close the window.

;;; Code:

(require 'posframe)
(require 'css-mode)
(require 'color-picker-canvas)

(defconst cpick-frame-width 340)
(defconst cpick-frame-height 415)

(defconst cpick-background 235)
(defconst cpick-background-hex-string
  (apply #'format "#%2x%2x%2x" (make-list 3 cpick-background)))
(defconst cpick-current-area-height 114)
(defconst cpick-old-area-height 32)
(defconst cpick-box-size 256)
(defconst cpick-box-x0 10)
(defconst cpick-box-y0 126)
(defconst cpick-box-x1 (+ cpick-box-x0 cpick-box-size -1))
(defconst cpick-box-y1 (+ cpick-box-y0 cpick-box-size -1))
(defconst cpick-box-y1+1 (+ cpick-box-y0 cpick-box-size))
(defconst cpick-hue-bar-x0 306)
(defconst cpick-hue-bar-x1 (+ cpick-hue-bar-x0 24 -1))
(defconst cpick-alpha-bar-x0 274)
(defconst cpick-alpha-bar-x1 (+ cpick-alpha-bar-x0 24 -1))
(defconst cpick-syntax-toggle-width (/ cpick-frame-width 5))
(defconst cpick-syntax-toggle-height 28)
(defconst cpick-syntax-toggle-y0 (- cpick-frame-height cpick-syntax-toggle-height))
(defconst cpick-syntax-background 190)
(defconst cpick-syntax-background-hex-string
  (apply #'format "#%2x%2x%2x" (make-list 3 cpick-syntax-background)))
(defconst cpick-syntax-rgb-x0 0)
(defconst cpick-syntax-rgb-x1 (+ cpick-syntax-rgb-x0 cpick-syntax-toggle-width))
(defconst cpick-syntax-hex-x0 (1+ cpick-syntax-rgb-x1))
(defconst cpick-syntax-hex-x1 (+ cpick-syntax-hex-x0 cpick-syntax-toggle-width))
(defconst cpick-syntax-hsl-x0 (1+ cpick-syntax-hex-x1))
(defconst cpick-syntax-hsl-x1 (+ cpick-syntax-hsl-x0 cpick-syntax-toggle-width))
(defconst cpick-syntax-hsv-x0 (1+ cpick-syntax-hsl-x1))
(defconst cpick-syntax-hsv-x1 (+ cpick-syntax-hsv-x0 cpick-syntax-toggle-width))
(defconst cpick-syntax-vec-x0 (1+ cpick-syntax-hsv-x1))
(defconst cpick-syntax-vec-x1 (+ cpick-syntax-vec-x0 cpick-syntax-toggle-width))

(defvar cpick-old-color-rgb [0 0 0 0.0])
(defvar cpick-old-color-hsl [0 0 0 0.0])
(defvar cpick-current-color [0 0 0 0.0])
(defvar cpick-hue-color [0 0 0 0.0])

;; One of (cpick-syntax-rgb cpick-syntax-rgba
;;         cpick-syntax-hex1 cpick-syntax-hexa1
;;         cpick-syntax-hex2 cpick-syntax-hexa2
;;         cpick-syntax-hsl cpick-syntax-hsla
;;         cpick-syntax-hsv cpick-syntax-vec)
(defvar cpick-old-syntax 'cpick-syntax-rgb)
(defvar cpick-current-syntax 'cpick-syntax-rgb)

;;; Utility

(defsubst cpick-clamp (x min max)
  (min (max x min) max))

(defsubst cpick-rgb-ftoi (f)
  "Convert a value between 0.0 and 1.0 to between 0 and 255."
  (round (* f 255.0)))

(defsubst cpick-rgb-itof (i)
  "Convert a value between 0.0 and 1.0 to between 0 and 255."
  (/ i 255.0))

(defsubst cpick-hue-ftoi (f)
  "Convert a value between 0.0 and 1.0 to between 0 and 360."
  (round (* f 360.0)))

(eval-and-compile
  (if (boundp 'replace-region-contents)
      (defalias 'cpick-replace-region-contents 'replace-region-contents)
    (defun cpick-replace-region-contents (beg end replace-fn)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (let ((repl (funcall replace-fn)))
            (let ((source-buffer (current-buffer)))
              (with-temp-buffer
                (insert repl)
                (let ((tmp-buffer (current-buffer)))
                  (set-buffer source-buffer)
                  (replace-buffer-contents tmp-buffer))))))))))

(eval-and-compile
  (if (= 1 (cdr (func-arity 'css--rgb-color)))
      (defalias 'cpick-css--rgb-color 'css--rgb-color)
    (cl-defun cpick-css--rgb-color (&optional include-alpha)
      "Copied from `css-mode.el'."
      (let ((result '())
            (iter 0))
        (while (< iter 4)
          (css--color-skip-blanks)
          (unless (looking-at css--number-or-percent-regexp)
            (cl-return-from cpick-css--rgb-color nil))
          (let* ((is-percent (match-beginning 1))
                 (str (match-string (if is-percent 1 2)))
                 (number (string-to-number str)))
            (if is-percent
                (setq number (* 255 (/ number 100.0)))
              (when (and include-alpha (= iter 3))
                (setq number (* number 255))))
            (push (min (max 0 (round number)) 255) result)
            (goto-char (match-end 0))
            (css--color-skip-blanks)
            (cl-incf iter)
            (when (and (= (skip-chars-forward ",/") 0)
                       (= iter 3))
              (cl-incf iter))
            (css--color-skip-blanks)))
        (when (looking-at ")")
          (forward-char)
          (apply #'format
                 (if (and include-alpha (= (length result) 4))
                     "#%02x%02x%02x%02x"
                   "#%02x%02x%02x")
                 (nreverse result)))))))

(cl-defun cpick-css--hsl-color (&optional need-alpha)
  "Copied from `css-mode.el' and modified."
  (let ((result '())
        alpha)
    (css--color-skip-blanks)
    (unless (looking-at css--angle-regexp)
      (cl-return-from cpick-css--hsl-color nil))
    (let ((hue (string-to-number (match-string 1)))
          (unit (match-string 2)))
      (goto-char (match-end 0))
      (cond
       ((or (not unit) (equal unit "deg"))
        (setq hue (/ hue 360.0)))
       ((equal unit "grad")
        (setq hue (/ hue 400.0)))
       ((equal unit "rad")
        (setq hue (/ hue (* 2 float-pi)))))
      (push (mod hue 1.0) result))
    (dotimes (_ 2)
      (skip-chars-forward ",")
      (css--color-skip-blanks)
      (unless (looking-at css--percent-regexp)
        (cl-return-from cpick-css--hsl-color nil))
      (let ((number (string-to-number (match-string 1))))
        (setq number (/ number 100.0))
        (push (min (max number 0.0) 1.0) result)
        (goto-char (match-end 0))
        (css--color-skip-blanks)))
    (css--color-skip-blanks)
    (when (> (skip-chars-forward ",/") 0)
      (css--color-skip-blanks)
      (unless (looking-at css--number-or-percent-regexp)
        (cl-return-from cpick-css--hsl-color nil))
      (let* ((is-percent (match-beginning 1))
             (str (match-string (if is-percent 1 2)))
             (number (string-to-number str)))
        (when is-percent
          (setq number (/ number 100.0)))
        (setq alpha (min (max 0.0 number) 1.0))
        (goto-char (match-end 0))
        (css--color-skip-blanks)))
    (when (looking-at ")")
      (forward-char)
      (let* ((hsl (setq cpick-old-color-hsl (vconcat (nreverse (cons 1.0 result)))))
             (rgb (cpick-hsl-to-rgb (aref hsl 0) (aref hsl 1) (aref hsl 2)))
             (r (cpick-rgb-ftoi (aref rgb 0)))
             (g (cpick-rgb-ftoi (aref rgb 1)))
             (b (cpick-rgb-ftoi (aref rgb 2))))
        (if (null alpha)
            (if need-alpha
                (format "#%02x%02x%02xff" r g b)
              (format "#%02x%02x%02x" r g b))
          (aset cpick-old-color-hsl 3 alpha)
          (format "#%02x%02x%02x%02x" r g b (cpick-rgb-ftoi alpha)))))))

(defsubst cpick--short-float-string (f)
  (if (< (- 1.0 1e-8) f)
      (format "%.1f" f)
    (substring (format "%.2f" f) 1)))

(defsubst cpick--percent-string (f)
  (format "%d%%" (truncate (+ (* f 100) 1e-12))))

(defsubst cpick-rgb-vec-to-hex-string (rgb)
  (format "#%02x%02x%02x" (aref rgb 0) (aref rgb 1) (aref rgb 2)))

(defsubst cpick-rgba-vec-to-hex-string (rgb &optional need-alpha)
  (if (or need-alpha
          (and (= (length rgb) 4)
               (< (aref rgb 3) 1.0)))
      (format "#%02x%02x%02x%02x" (aref rgb 0) (aref rgb 1) (aref rgb 2)
              (cpick-rgb-ftoi (aref rgb 3)))
    (format "#%02x%02x%02x" (aref rgb 0) (aref rgb 1) (aref rgb 2))))

(defsubst cpick-rgba-vec-to-string (rgb &optional need-alpha)
  (if (or need-alpha
          (and (= (length rgb) 4)
               (< (aref rgb 3) 1.0)))
      (format "rgba(%d, %d, %d, %s)" (aref rgb 0) (aref rgb 1) (aref rgb 2)
              (cpick--short-float-string (aref rgb 3)))
    (format "rgb(%d, %d, %d)" (aref rgb 0) (aref rgb 1) (aref rgb 2))))

(defsubst cpick-hsla-vec-to-string (hsl &optional need-alpha)
  (if (or need-alpha
          (and (= (length hsl) 4)
               (< (aref hsl 3) 1.0)))
      (format "hsla(%d, %s, %s, %s)"
              (cpick-hue-ftoi (aref hsl 0))
              (cpick--percent-string (aref hsl 1))
              (cpick--percent-string (aref hsl 2))
              (cpick--short-float-string (aref hsl 3)))
    (format "hsl(%s, %s, %s)"
            (cpick-hue-ftoi (aref hsl 0))
            (cpick--percent-string (aref hsl 1))
            (cpick--percent-string (aref hsl 2)))))

(defsubst cpick-hsv-vec-to-string (hsv)
  (if (and (= (length hsv) 4)
           (< (aref hsv 3) 1.0))
      (format "hsva(%d, %s, %s, %s)"
              (round (aref hsv 0))
              (cpick--percent-string (aref hsv 1))
              (cpick--percent-string (aref hsv 2))
              (cpick--short-float-string (aref hsv 3)))
    (format "hsv(%s, %s, %s)"
            (round (aref hsv 0))
            (cpick--percent-string (aref hsv 1))
            (cpick--percent-string (aref hsv 2)))))

(defsubst cpick-rgb-vec256-to-vec-string (rgb)
  (let ((r (cpick-rgb-itof (aref rgb 0)))
        (g (cpick-rgb-itof (aref rgb 1)))
        (b (cpick-rgb-itof (aref rgb 2)))
        (a (aref rgb 3)))
    (if (and (= (length rgb) 4)
             (< a 1.0))
        (format "vec4(%.2g, %.2g, %.2g, %.2f)" r g b a)
      (format "vec3(%.2g, %.2g, %.2g)" r g b))))

(defsubst cpick-color-vec-alpha-p (vec)
  (< 3 (length vec)))

(defun cpick-rgba-to-hsv (r g b a)
  (let* ((max (max r g b))
         (min (min r g b))
         (max-min (- max min)))
    (if (< max-min 1e-8)
        (vector 0.0 0.0 min a)
      (vector
       (cond ((and (= r max) (>= g b))
              (* 60 (/ (- g b) max-min)))
             ((and (= r max) (< g b))
              (+ 360 (* 60 (/ (- g b) max-min))))
             ((= max g)
              (+ 120 (* 60 (/ (- b r) max-min))))
             ((= max b)
              (+ 240 (* 60 (/ (- r g) max-min)))))
       (if (= max 0) 0 (- 1 (/ min max)))
       max
       a))))

(defsubst cpick-rgba-vec256-to-hsv (rgba)
  (cpick-rgba-to-hsv (cpick-rgb-itof (aref rgba 0))
                     (cpick-rgb-itof (aref rgba 1))
                     (cpick-rgb-itof (aref rgba 2))
                     (aref rgba 3)))

(defun cpick-rgba-to-hsl (r g b a)
  (let* ((max (max r g b))
         (min (min r g b))
         (delta (- max min))
         (l (/ (+ max min) 2.0)))
    (if (= delta 0)
        (vector 0.0 0.0 l a)
      (let* ((s (if (<= l 0.5) (/ delta (+ max min))
                  (/ delta (- 2.0 max min))))
             (rc (/ (- max r) delta))
             (gc (/ (- max g) delta))
             (bc (/ (- max b) delta))
             (h  (mod
                  (/
                   (cond
                    ((= r max) (- bc gc))
                    ((= g max) (+ 2.0 rc (- bc)))
                    (t (+ 4.0 gc (- rc))))
                   6.0)
                  1.0)))
        (vector h s l a)))))

(defsubst cpick-rgba-vec256-to-hsl (rgba)
  (cpick-rgba-to-hsl (cpick-rgb-itof (aref rgba 0))
                     (cpick-rgb-itof (aref rgba 1))
                     (cpick-rgb-itof (aref rgba 2))
                     (aref rgba 3)))

(defun cpick-hsl-to-rgb (h s l)
  (if (= s 0.0)
      (vector l l l)
    (let* ((m2 (if (<= l 0.5)
                   (* l (+ 1.0 s))
                 (- (+ l s) (* l s))))
           (m1 (- (* 2.0 l) m2)))
      (vector
       (color-hue-to-rgb m1 m2 (mod (+ h (/ 3.0)) 1))
       (color-hue-to-rgb m1 m2 h)
       (color-hue-to-rgb m1 m2 (mod (- h (/ 3.0)) 1))))))

(defun cpick-hsv-to-rgb (h s v)
  (let* ((c (* s v))
         (x (* c (- 1 (abs (1- (mod (/ h 60) 2))))))
         (m (- v c))
         r1 g1 b1)
    (cond
     ((and (<= 0 h) (<= h 60))
      (setq r1 c
            g1 x
            b1 0))
     ((and (< 60 h) (<= h 120))
      (setq r1 x
            g1 c
            b1 0))
     ((and (< 120 h) (<= h 180))
      (setq r1 0
            g1 c
            b1 x))
     ((and (< 180 h) (<= h 240))
      (setq r1 0
            g1 x
            b1 c))
     ((and (< 240 h) (<= h 300))
      (setq r1 x
            g1 0
            b1 c))
     (t
      (setq r1 c
            g1 0
            b1 x)))
    (vector (+ r1 m) (+ g1 m) (+ b1 m))))

(defsubst cpick--one-digit-to-two-digit (c)
  "f -> ff"
  (let ((i (if (<= ?0 c ?9)
               (- c ?0)
             ;; (<= ?a c ?f)
             (- c 87))))
    (+ (* i 16) i)))

(defun cpick-hex-string-to-color-vec (hex)
  (cl-case (length hex)
    (4
     (vector (cpick--one-digit-to-two-digit (aref hex 1))
             (cpick--one-digit-to-two-digit (aref hex 2))
             (cpick--one-digit-to-two-digit (aref hex 3))
             1.0))
    (5
     (vector (cpick--one-digit-to-two-digit (aref hex 1))
             (cpick--one-digit-to-two-digit (aref hex 2))
             (cpick--one-digit-to-two-digit (aref hex 3))
             (cpick-rgb-itof (cpick--one-digit-to-two-digit (aref hex 4)))))
    (7
     (vector (string-to-number (substring hex 1 3) 16)
             (string-to-number (substring hex 3 5) 16)
             (string-to-number (substring hex 5 7) 16)
             1.0))
    (9
     (vector (string-to-number (substring hex 1 3) 16)
             (string-to-number (substring hex 3 5) 16)
             (string-to-number (substring hex 5 7) 16)
             (cpick-rgb-itof (string-to-number (substring hex 7 9) 16))))))

(defun cpick-color-vec-to-string (&optional old-color old-syntax)
  (let ((color (if old-color cpick-old-color-rgb cpick-current-color))
        (syntax (if old-syntax cpick-old-syntax cpick-current-syntax)))
    (cl-case syntax
      ;; #3a3
      (cpick-syntax-hex1
       (cpick-rgba-vec-to-hex-string color))
      ;; #3a38
      (cpick-syntax-hexa1
       (cpick-rgba-vec-to-hex-string color t))
      ;; #33aa33
      (cpick-syntax-hex2
       (cpick-rgba-vec-to-hex-string color))
      ;; #33aa3380
      (cpick-syntax-hexa2
       (cpick-rgba-vec-to-hex-string color t))
      ;; rgb(51, 170, 51)
      (cpick-syntax-rgb
       (cpick-rgba-vec-to-string color))
      ;; rgba(51, 170, 51, .5)
      (cpick-syntax-rgba
       (cpick-rgba-vec-to-string color t))
      ;; hsl(270, 60%, 70%)
      (cpick-syntax-hsl
       ;; if old-color is non-nil,
       ;; returns a value without calculating so that there is no calculation error.
       (cpick-hsla-vec-to-string (if old-color
                                     cpick-old-color-hsl
                                   (cpick-rgba-vec256-to-hsl color))))
      ;; hsla(270, 60%, 70%, .5)
      (cpick-syntax-hsla
       ;; if old-color is non-nil,
       ;; returns a value without calculating so that there is no calculation error.
       (cpick-hsla-vec-to-string (if old-color
                                     cpick-old-color-hsl
                                   (cpick-rgba-vec256-to-hsl color))
                                 t))
      ;; hsv(270, 60%, 70%)
      (cpick-syntax-hsv
       (cpick-hsv-vec-to-string (cpick-rgba-vec256-to-hsv color)))
      ;; vec3(.3, .1, .2)
      (cpick-syntax-vec
       (cpick-rgb-vec256-to-vec-string color)))))

;;; Buffer opration

(defvar cpick-parent-buffer nil)

(defvar cpick-color-string-beg 0)
(defvar cpick-color-string-end 0)

(defun cpick-css--compute-color (start-point match)
  "Copied from `css-mode.el' and modified."
  (cond
   ;; #3a3
   ((and (eq (aref match 0) ?#)
         (= (length match) 3))
    (setq cpick-current-syntax 'cpick-syntax-hex1
          cpick-old-syntax 'cpick-syntax-hex1)
    match)
   ;; #3a38
   ((and (eq (aref match 0) ?#)
         (= (length match) 4))
    (setq cpick-current-syntax 'cpick-syntax-hexa1
          cpick-old-syntax 'cpick-syntax-hexa1)
    match)
   ;; #33aa33
   ((and (eq (aref match 0) ?#)
         (= (length match) 7))
    (setq cpick-current-syntax 'cpick-syntax-hex2
          cpick-old-syntax 'cpick-syntax-hex2)
    match)
   ;; #33aa3380
   ((and (eq (aref match 0) ?#)
         (= (length match) 9))
    (setq cpick-current-syntax 'cpick-syntax-hexa2
          cpick-old-syntax 'cpick-syntax-hexa2)
    match)
   ;; rgba(51, 170, 51)
   ((string= match "rgb(")
    (setq cpick-current-syntax 'cpick-syntax-rgb
          cpick-old-syntax 'cpick-syntax-rgb)
    (cpick-css--rgb-color t))
   ;; rgba(51, 170, 51, .5)
   ((string= match "rgba(")
    (setq cpick-current-syntax 'cpick-syntax-rgba
          cpick-old-syntax 'cpick-syntax-rgba)
    (cpick-css--rgb-color t))
   ;; hsl(270, 60%, 70%)
   ((string= match "hsl(")
    (setq cpick-current-syntax 'cpick-syntax-hsl
          cpick-old-syntax 'cpick-syntax-hsl)
    (cpick-css--hsl-color))
   ;; hsl(270, 60%, 70%, .5)
   ((string= match "hsla(")
    (setq cpick-current-syntax 'cpick-syntax-hsla
          cpick-old-syntax 'cpick-syntax-hsla)
    (cpick-css--hsl-color t))
   (t
    (setq cpick-current-syntax 'cpick-syntax-rgb
          cpick-old-syntax 'cpick-syntax-rgb)
    (css--named-color start-point match))))

(defun cpick-buffer-search-color ()
  "This code was written with reference to `css--fontify-region'."
  (cl-block nil
    (save-excursion
      (let ((case-fold-search t)
            (point (point))
            (end (line-end-position)))
        (move-beginning-of-line nil)
        (while (re-search-forward css--colors-regexp end t)
          ;; Skip comments and strings.
          (unless (nth 8 (syntax-ppss))
            (let* ((start (match-beginning 0))
                   (hexstr (cpick-css--compute-color start (match-string-no-properties 0))))
              (when (and hexstr (<= start point (1- (point))))
                (setq cpick-old-color-rgb (cpick-hex-string-to-color-vec hexstr)
                      cpick-current-color cpick-old-color-rgb
                      cpick-color-string-beg start
                      cpick-color-string-end (point))
                (cl-return cpick-old-color-rgb)))))))))

(defun cpick-buffer-write-color ()
  (unless (equal cpick-old-color-rgb cpick-current-color)
    (with-current-buffer cpick-parent-buffer
      (cpick-replace-region-contents cpick-color-string-beg
                                     cpick-color-string-end
                                     #'cpick-color-vec-to-string))))

;;; Cursor

(defvar cpick-cursor-box-xpm
  "/* XPM */
static char * cursor_xpm[] = {
\"12 12 9 1\",
\" 	c None\",
\".	c #8B8B8B\",
\"+	c #CACACA\",
\"@	c #F5F5F5\",
\"#	c #FFFFFF\",
\"$	c #E0E0E0\",
\"%	c #FCFCFC\",
\"&	c #FDFDFD\",
\"*	c #BDBDBD\",
\"   ......   \",
\"  .+@##@+.  \",
\" .$##%%##$. \",
\".+#&*..*&#+.\",
\".@#*.  .*#@.\",
\".#%.    .%#.\",
\".#%.    .%#.\",
\".@#*.  .*#@.\",
\".+#&*..*&#+.\",
\" .$##%%##$. \",
\"  .+@##@+.  \",
\"   ......   \"};")
(defvar cpick-cursor-box-width 0)
(defvar cpick-cursor-box-height 0)
(defvar cpick-cursor-box-colors nil)
(defvar cpick-cursor-box-shape nil)

(defun cpick-cursor-box-read-xpm (data)
  (unless cpick-cursor-box-shape
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (let ((ncolors 0)
            (index 1)
            colors)
        (when (re-search-forward "\"\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)\"" nil t)
          (setq cpick-cursor-box-width (string-to-number (match-string 1))
                cpick-cursor-box-height (string-to-number (match-string 2))
                ncolors (string-to-number (match-string 3))
                cpick-cursor-box-colors (make-vector ncolors 0)
                cpick-cursor-box-shape (make-vector (* cpick-cursor-box-width cpick-cursor-box-height)
                                                    0))
          (dotimes (_ ncolors)
            (when (re-search-forward "\"\\(.\\).*c \\(.*\\)\"" nil t)
              (let ((color (match-string 2)))
                (cond
                 ((string= color "None")
                  (aset cpick-cursor-box-colors 0 [0 0 0 0])
                  (push (cons (aref (match-string 1) 0) 0) colors))
                 ((= (aref color 0) ?#)
                  (aset cpick-cursor-box-colors index (cpick-hex-string-to-color-vec (match-string 2)))
                  (push (cons (aref (match-string 1) 0) index) colors)
                  (cl-incf index))))))
          (dotimes (y cpick-cursor-box-height)
            (when (re-search-forward "\"\\(.*\\)\"" nil t)
              (cl-loop for c across (match-string 1)
                       for i from (* y cpick-cursor-box-width)
                       do (aset cpick-cursor-box-shape i (cdr (assq c colors)))))))))))

(defsubst cpick-cursor-inside-box-p (x y)
  (and (<= cpick-box-x0 x cpick-box-x1)
       (<= cpick-box-y0 y cpick-box-y1)))

(defsubst cpick-cursor-inside-hue-bar-p (x y)
  (and (<= cpick-hue-bar-x0 x cpick-hue-bar-x1)
       (<= cpick-box-y0 y cpick-box-y1)))

(defsubst cpick-cursor-inside-alpha-bar-p (x y)
  (and (<= cpick-alpha-bar-x0 x cpick-alpha-bar-x1)
       (<= cpick-box-y0 y cpick-box-y1)))

(defvar cpick-cursor-box-x 0)
(defvar cpick-cursor-box-y 0)

(defsubst cpick-cursor-box-draw (drawable snapshot x0 y0)
  (let* ((x (cpick-clamp x0 cpick-box-x0 cpick-box-x1))
         (y (cpick-clamp y0 cpick-box-y0 cpick-box-y1)))
    (when snapshot
      (let* ((sx (- cpick-cursor-box-x (/ cpick-cursor-box-width 2)))
             (sy (- cpick-cursor-box-y (/ cpick-cursor-box-height 2)))
             (ex (+ sx cpick-cursor-box-width))
             (ey (+ sy cpick-cursor-box-height)))
        (cpick-drawable-restore-rect drawable snapshot sx sy ex ey)))
    (setq cpick-cursor-box-x x
          cpick-cursor-box-y y
          cpick-current-color (cpick-drawable-get-color drawable x y (aref cpick-current-color 3)))
    (cpick-with-drawable (bitmap offset width) drawable
      (let* ((colors cpick-cursor-box-colors)
             (shape cpick-cursor-box-shape)
             (sx (- x (/ cpick-cursor-box-width 2)))
             (y (- y (/ cpick-cursor-box-height 2)))
             (ex (+ sx cpick-cursor-box-width))
             (ey (+ y cpick-cursor-box-height))
             (i 0))
        (while (< y ey)
          (let ((x sx)
                (offx (* y width)))
            (while (< x ex)
              (let ((color-index (aref shape i)))
                (when (/= 0 color-index)
                  (let ((color (aref colors color-index)))
                    (when (cpick-cursor-inside-box-p x y)
                      (cpick-bitmap-set-pixel-linear bitmap offset (+ x offx)
                                                     (aref color 0)
                                                     (aref color 1)
                                                     (aref color 2))))))
              (cl-incf x)
              (cl-incf i)))
          (cl-incf y))))))

(defconst cpick-cursor-gray-scale 240)

(defsubst cpick-cursor-fill-rectangle (bitmap offset width x0 y0 x1 y1)
  (while (<= y0 y1)
    (let ((x x0)
          (offx (* y0 width)))
      (while (<= x x1)
        (cpick-bitmap-set-pixel-linear bitmap offset (+ x offx)
                                       cpick-cursor-gray-scale
                                       cpick-cursor-gray-scale
                                       cpick-cursor-gray-scale)
        (cl-incf x)))
    (cl-incf y0)))

(defun cpick-cursor-slider-draw (drawable x0 y0 x1 y1)
  (cpick-with-drawable (bitmap offset width) drawable
    (cpick-cursor-fill-rectangle bitmap offset width x0 y0 x1 (1+ y0))
    (cpick-cursor-fill-rectangle bitmap offset width x0 (1- y1) x1 y1)
    (cpick-cursor-fill-rectangle bitmap offset width x0 (1+ y0) (+ x0 1) (1- y1))
    (cpick-cursor-fill-rectangle bitmap offset width (1- x1) (1+ y0) x1 (1- y1))))

(defvar cpick-cursor-hue-bar-y 0)

(defsubst cpick-cursor-hue-bar-draw (drawable snapshot y0)
  (when snapshot
    (let* ((x0 cpick-hue-bar-x0)
           (x1 cpick-hue-bar-x1)
           (yt (- cpick-cursor-hue-bar-y 3))
           (yb (+ cpick-cursor-hue-bar-y 3)))
      (cpick-drawable-restore-rect drawable snapshot x0 yt x1 yb)))
  (setq cpick-cursor-hue-bar-y y0)
  (let* ((x0 (1+ cpick-hue-bar-x0))
         (x1 (1- cpick-hue-bar-x1))
         (yt (- y0 2))
         (yb (+ y0 2)))
    (cpick-cursor-slider-draw drawable x0 yt x1 yb)))

(defvar cpick-cursor-alpha-bar-y 0)

(defsubst cpick-cursor-alpha-bar-draw (drawable snapshot y0)
  (when snapshot
    (let* ((x0 cpick-alpha-bar-x0)
           (x1 cpick-alpha-bar-x1)
           (yt (- cpick-cursor-alpha-bar-y 3))
           (yb (+ cpick-cursor-alpha-bar-y 3)))
      (cpick-drawable-restore-rect drawable snapshot x0 yt x1 yb)))
  (setq cpick-cursor-alpha-bar-y y0)
  (let* ((x0 (1+ cpick-alpha-bar-x0))
         (x1 (1- cpick-alpha-bar-x1))
         (yt (- y0 2))
         (yb (+ y0 2)))
    (cpick-cursor-slider-draw drawable x0 yt x1 yb)))

;;; Frame

(defvar cpick-frame-base nil)
(defvar cpick-frame-current nil)
(defvar cpick-frame-old nil)
(defvar cpick-frame-current-string nil)
(defvar cpick-frame-old-string nil)
(defvar cpick-frame-syntax-toggle nil)
(defvar cpick-frame-syntax-rgb-string nil)
(defvar cpick-frame-syntax-hex-string nil)
(defvar cpick-frame-syntax-hsl-string nil)
(defvar cpick-frame-syntax-hsv-string nil)
(defvar cpick-frame-syntax-vec-string nil)

(defvar cpick-buffer-base nil)
(defvar cpick-buffer-current-string nil)
(defvar cpick-buffer-old-string nil)

(defun cpick--create-posframe (buffer-or-name override-parameters &optional handler string)
  "`posframe--create-posframe' wrapper function."
  (let ((frame-resize-pixelwise t)
        (buffer (get-buffer-create buffer-or-name))
        (parent-frame (window-frame (selected-window))))
    (with-current-buffer buffer
      (let ((frame (posframe--create-posframe
                    buffer
                    :parent-frame parent-frame
                    :override-parameters override-parameters)))
        (when string
          (posframe--insert-string string t))
        (when handler
          (funcall handler frame buffer))
        frame))))

(defun cpick-frame-create ()
  (cpick--create-posframe "*cpick-base*"
                          '((parent-frame . nil)
                            (no-accept-focus . nil)
                            (visibility . nil))
                          (lambda (frame buffer)
                            (setq cpick-frame-base frame
                                  cpick-buffer-base buffer)))
  (set-frame-size cpick-frame-base cpick-frame-width cpick-frame-height t)

  (cpick--create-posframe "*cpick-current-area*"
                          `((parent-frame . ,cpick-frame-base)
                            (no-accept-focus . nil)
                            (visibility . t))
                          (lambda (frame buffer)
                            (setq cpick-frame-current frame)
                            (with-current-buffer buffer
                              (local-set-key [down-mouse-1] (lambda ()
                                                              (interactive)
                                                              (cpick-buffer-write-color)
                                                              (cpick-frame-delete))))))
  (set-frame-size cpick-frame-current cpick-frame-width cpick-current-area-height t)

  (cpick--create-posframe "*cpick-current-string*"
                          `((parent-frame . ,cpick-frame-base)
                            (no-accept-focus . nil)
                            (visibility . t))
                          (lambda (frame buffer)
                            (setq cpick-frame-current-string frame
                                  cpick-buffer-current-string buffer)
                            (with-current-buffer buffer
                              (local-set-key [down-mouse-1] (lambda ()
                                                              (interactive)
                                                              (cpick-buffer-write-color)
                                                              (cpick-frame-delete))))))
  (set-face-attribute 'default cpick-frame-current-string :width 'normal :height 140 :weight 'bold)

  (cpick--create-posframe "*cpick-old-area*"
                          `((parent-frame . ,cpick-frame-base)
                            (no-accept-focus . nil)
                            (visibility . t))
                          (lambda (frame buffer)
                            (setq cpick-frame-old frame)
                            (with-current-buffer buffer
                              (local-set-key [down-mouse-1] (lambda ()
                                                              (interactive)
                                                              (cpick-frame-delete))))))
  (set-frame-size cpick-frame-old cpick-frame-width cpick-old-area-height t)

  (cpick--create-posframe "*cpick-old-string*"
                          `((parent-frame . ,cpick-frame-base)
                            (no-accept-focus . nil)
                            (visibility . t))
                          (lambda (frame buffer)
                            (setq cpick-frame-old-string frame
                                  cpick-buffer-old-string buffer)
                            (with-current-buffer buffer
                              (local-set-key [down-mouse-1] (lambda ()
                                                              (interactive)
                                                              (cpick-frame-delete))))))
  (set-face-attribute 'default cpick-frame-old-string :width 'normal :height 100 :weight 'bold)

  (cpick--create-posframe "*cpick-syntax-toggle-area*"
                          `((parent-frame . ,cpick-frame-base)
                            (no-accept-focus . nil)
                            (background-color . ,cpick-background-hex-string)
                            (visibility . t))
                          (lambda (frame buffer)
                            (setq cpick-frame-syntax-toggle frame)
                            (with-current-buffer buffer
                              (local-set-key "q" (lambda () (interactive) (cpick-frame-delete))))))
  (set-frame-size cpick-frame-syntax-toggle cpick-syntax-toggle-width cpick-syntax-toggle-height t)
  (set-frame-position cpick-frame-syntax-toggle 0 cpick-syntax-toggle-y0)

  (let ((x 0)
        (y (+ cpick-syntax-toggle-y0 5))
        (labels '("rgb" "hex" "hsl" "hsv" "vec"))
        (symbols '(cpick-frame-syntax-rgb-string
                   cpick-frame-syntax-hex-string
                   cpick-frame-syntax-hsl-string
                   cpick-frame-syntax-hsv-string
                   cpick-frame-syntax-vec-string))
        (syntaxes '(cpick-syntax-rgb cpick-syntax-hex2 cpick-syntax-hsl
                                     cpick-syntax-hsv cpick-syntax-vec)))
    (dolist (label labels)
      (let* ((syntax (pop syntaxes))
             (symbol (pop symbols))
             (frame (cpick--create-posframe
                     (concat "*cpick-syntax-" label "-string*")
                     `((parent-frame . ,cpick-frame-base)
                       (no-accept-focus . nil)
                       (foreground-color . "gray30")
                       (background-color . ,cpick-syntax-background-hex-string)
                       (width . 3)
                       (visibility . t))
                     (lambda (frame buffer)
                       (set symbol frame)
                       (with-current-buffer buffer
                         (local-set-key "q" (lambda () (interactive) (cpick-frame-delete)))
                         (local-set-key [down-mouse-1] (lambda ()
                                                         (interactive)
                                                         (cpick-change-syntax syntax)))))
                     label)))
        (set-face-attribute 'default frame :width 'normal :height 100 :weight 'bold)
        (set-frame-position frame
                            (+ x (/ (- cpick-syntax-toggle-width
                                       (* (frame-char-width frame) 3))
                                    2))
                            y)
        (cl-incf x cpick-syntax-toggle-width)))))

(defun cpick-frame-show-all ()
  (dolist (frame (list cpick-frame-base
                       cpick-frame-current
                       cpick-frame-old
                       cpick-frame-current-string
                       cpick-frame-old-string))
    (unless (frame-visible-p frame)
      (make-frame-visible frame))))

(defun cpick-frame-delete ()
  (interactive)
  (with-current-buffer cpick-parent-buffer
    (cpick-minor-mode -1))
  (posframe-hide cpick-buffer-base)
  ;; (kill-buffer cpick-buffer-base)
  )

(defun cpick-change-syntax (&optional syntax)
  (when syntax
    (setq cpick-current-syntax syntax))
  (let ((frames (vector cpick-frame-syntax-rgb-string
                        cpick-frame-syntax-hex-string
                        cpick-frame-syntax-hsl-string
                        cpick-frame-syntax-hsv-string
                        cpick-frame-syntax-vec-string))
        (index (cl-case cpick-current-syntax
                 ((cpick-syntax-rgb cpick-syntax-rgba)
                  0)
                 ((cpick-syntax-hex1 cpick-syntax-hexa1 cpick-syntax-hex2 cpick-syntax-hexa2)
                  1)
                 ((cpick-syntax-hsl cpick-syntax-hsla)
                  2)
                 (cpick-syntax-hsv
                  3)
                 (cpick-syntax-vec
                  4))))
    (dotimes (i (length frames))
      (let ((frame (aref frames i)))
        (modify-frame-parameters frame
                                 `((background-color . ,(if (= i index)
                                                            cpick-background-hex-string
                                                          cpick-syntax-background-hex-string))))
        (raise-frame frame)))
    (set-frame-position cpick-frame-syntax-toggle
                        (* cpick-syntax-toggle-width index)
                        cpick-syntax-toggle-y0))
  (cpick-draw-current-area))

;;; Draw functions

(defun cpick-draw-old-area ()
  (let* ((changed (not (equal cpick-old-color-rgb cpick-current-color)))
         (hex (cpick-rgb-vec-to-hex-string cpick-old-color-rgb))
         (str (cpick-color-vec-to-string t t))
         (alpha (aref cpick-old-color-rgb 3))
         (frame-alpha-lower-limit 0))
    (modify-frame-parameters cpick-frame-old `((background-color . ,hex)
                                               (alpha . ,alpha)))
    (modify-frame-parameters cpick-frame-old-string
                             `((foreground-color . ,(css--contrasty-color hex))
                               (background-color . ,hex)
                               (width . ,(length str))
                               ;; A workaround because the text is also alpha...
                               (alpha . ,(max alpha 0.9))))
    (cond
     (changed
      ;; Changing visibility is very buggy... (12/18/2019)
      ;; (make-frame-visible cpick-frame-old)
      ;; (make-frame-visible cpick-frame-old-string)
      (set-frame-position cpick-frame-old
                          0 (- cpick-current-area-height cpick-old-area-height))
      (set-frame-position cpick-frame-old-string
                          (/ (- cpick-frame-width
                                (* (frame-char-width cpick-frame-old-string) (length str)))
                             2)
                          (+ (- cpick-current-area-height cpick-old-area-height) 10)))
     (t
      ;; (make-frame-invisible cpick-frame-old)
      ;; (make-frame-invisible cpick-frame-old-string)
      (set-frame-position cpick-frame-old 1024 1024)
      (set-frame-position cpick-frame-old-string 1024 1024)))
    (with-selected-frame cpick-frame-old-string
      (with-current-buffer cpick-buffer-old-string
        (posframe--insert-string str t)))))

(defun cpick-draw-current-area ()
  (let* ((changed (not (equal cpick-old-color-rgb cpick-current-color)))
         (hex (cpick-rgb-vec-to-hex-string cpick-current-color))
         (str (cpick-color-vec-to-string (not changed)))
         (alpha (aref cpick-current-color 3))
         (frame-alpha-lower-limit 0)
         (height (if changed
                     (- cpick-current-area-height cpick-old-area-height)
                   cpick-current-area-height)))
    (modify-frame-parameters cpick-frame-current `((background-color . ,hex)
                                                   (alpha . ,alpha)))
    (modify-frame-parameters cpick-frame-current-string
                             `((foreground-color . ,(css--contrasty-color hex))
                               (background-color . ,hex)
                               (width . ,(length str))
                               ;; A workaround because the text is also alpha...
                               (alpha . ,(max alpha 0.9))))
    (set-frame-size cpick-frame-current cpick-frame-width height t)
    (cpick-draw-old-area)
    (set-frame-position cpick-frame-current-string
                        (/ (- cpick-frame-width
                              (* (frame-char-width cpick-frame-current-string) (length str)))
                           2)
                        (if changed (- 40 (/ (- cpick-current-area-height height) 2)) 40))
    (raise-frame cpick-frame-current-string)
    (with-selected-frame cpick-frame-current-string
      (with-current-buffer cpick-buffer-current-string
        (if changed
            (posframe--insert-string str t)
          (posframe--insert-string str t))))))

(defun cpick-draw-box-cursor (drawable snapshot x0 y0)
  (cpick-cursor-box-draw drawable snapshot x0 y0)
  (cpick-draw-current-area)
  (cpick-draw-alpha-bar drawable)
  (cpick-cursor-alpha-bar-draw drawable nil cpick-cursor-alpha-bar-y))

(defun cpick-draw-box (drawable &optional snapshot)
  (cpick-with-drawable (bitmap offset) drawable
    (setq bitmap (or snapshot bitmap))
    (let* ((pr (aref cpick-hue-color 0))
           (pg (aref cpick-hue-color 1))
           (pb (aref cpick-hue-color 2))
           (i (+ cpick-box-x0 (* cpick-box-y0 cpick-frame-width)))
           (inc-i (- cpick-frame-width 256))
           (end-i 0)
           (ry 1.0)
           (ryfx 0.0)
           (rywx 0.0)
           (inc (/ 1.0 255.0))
           (inc-ryfx 0.0))
      (while (< 0.0 ry)
        (setq end-i (+ i 256)
              ryfx 0.0
              rywx (* ry 255.0)
              inc-ryfx (* inc ry))
        (while (< i end-i)
          (cpick-bitmap-set-pixel-linear bitmap offset i
                                         (round (+ rywx (* ryfx pr)))
                                         (round (+ rywx (* ryfx pg)))
                                         (round (+ rywx (* ryfx pb))))
          (cl-incf ryfx inc-ryfx)
          (cl-decf rywx ry)
          (cl-incf i))
        (cl-incf i inc-i)
        (cl-decf ry inc)))))

(defun cpick-draw-hue-bar-cursor (drawable snapshot _ y0)
  (let* ((y (cpick-clamp y0 cpick-box-y0 cpick-box-y1+1))
         (ry (- 1.0 (/ (- y cpick-box-y0)
                       (float cpick-box-size))))
         (rgb (cpick-hsv-to-rgb (* 360.0 ry) 1.0 1.0)))
    (cpick-cursor-hue-bar-draw drawable snapshot y)
    (aset cpick-hue-color 0 (cpick-rgb-ftoi (aref rgb 0)))
    (aset cpick-hue-color 1 (cpick-rgb-ftoi (aref rgb 1)))
    (aset cpick-hue-color 2 (cpick-rgb-ftoi (aref rgb 2)))
    (cpick-draw-box drawable)
    (cpick-draw-box-cursor drawable nil cpick-cursor-box-x cpick-cursor-box-y)))

(defun cpick-draw-hue-bar (drawable)
  (cpick-with-drawable (bitmap offset) drawable
    (let ((x 0)
          (y 0)
          (width cpick-frame-width)
          (offx cpick-hue-bar-x0)
          (offy cpick-box-y0))
      (while (< y 256)
        (setq x 0)
        (let* ((fy (/ y 255.0))
               (ry (- 1.0 fy))
               (rgb (cpick-hsv-to-rgb (* 360.0 ry) 1.0 1.0)))
          (while (< x 24)
            (cpick-bitmap-set-pixel-linear bitmap offset
                                           (+ (* (+ y offy) width) x offx)
                                           (cpick-rgb-ftoi (aref rgb 0))
                                           (cpick-rgb-ftoi (aref rgb 1))
                                           (cpick-rgb-ftoi (aref rgb 2)))
            (cl-incf x)))
        (cl-incf y)))))

(defun cpick-draw-alpha-bar-cursor (drawable snapshot _ y0)
  (let* ((y (cpick-clamp y0 cpick-box-y0 cpick-box-y1+1))
         (ry (- 1.0 (/ (- y cpick-box-y0)
                       (float cpick-box-size)))))
    (aset cpick-current-color 3 ry)
    (cpick-cursor-alpha-bar-draw drawable snapshot y)
    (cpick-draw-current-area)))

(defconst cpick-checkered-pattern-col1 [102 102 102])
(defconst cpick-checkered-pattern-col2 [153 153 153])

(defun cpick-draw-alpha-bar (drawable &optional snapshot)
  (cpick-with-drawable (bitmap offset) drawable
    (setq bitmap (or snapshot bitmap))
    (let* ((i (+ cpick-alpha-bar-x0 (* cpick-box-y0 cpick-frame-width)))
           (inc-i (- cpick-frame-width 24))
           (end-i 0)
           (inc (/ 1.0 256.0))
           (x 0)
           (y 0)
           (fy 0.0)
           (ry 1.0)
           (pr (aref cpick-current-color 0))
           (pg (aref cpick-current-color 1))
           (pb (aref cpick-current-color 2))
           (col1 cpick-checkered-pattern-col1)
           (col2 cpick-checkered-pattern-col2))
      (while (> ry 0.0)
        (setq end-i (+ i 24)
              x 0)
        (while (< i end-i)
          (let ((col (if (= (logand x #b1000)
                            (logand y #b1000))
                         col1 col2)))
            (cpick-bitmap-set-pixel-linear bitmap offset i
                                           (round (+ (* fy (aref col 0))
                                                     (* ry pr)))
                                           (round (+ (* fy (aref col 1))
                                                     (* ry pg)))
                                           (round (+ (* fy (aref col 2))
                                                     (* ry pb)))))
          (cl-incf x)
          (cl-incf i))
        (cl-incf y)
        (cl-incf i inc-i)
        (cl-incf fy inc)
        (cl-decf ry inc)))))

(defun cpick-draw-checkered-pattern (drawable)
  (cpick-with-drawable (bitmap offset) drawable
    (let ((x 0)
          (y 0)
          (ex cpick-frame-width)
          (ey cpick-current-area-height)
          (width cpick-frame-width)
          (col1 cpick-checkered-pattern-col1)
          (col2 cpick-checkered-pattern-col2))
      (while (< y ey)
        (setq x 0)
        (while (< x ex)
          (let ((col (if (= (logand x #b1000)
                            (logand y #b1000))
                         col1 col2)))
            (cpick-bitmap-set-pixel-linear bitmap offset
                                           (+ (* y width) x)
                                           (aref col 0)
                                           (aref col 1)
                                           (aref col 2)))
          (cl-incf x))
        (cl-incf y)))))

(defun cpick-draw-syntax-toggle-area (drawable)
  (cpick-with-drawable (bitmap offset) drawable
    (let ((x 0)
          (y cpick-syntax-toggle-y0)
          (ex cpick-frame-width)
          (ey cpick-frame-height)
          (width cpick-frame-width))
      (while (< y ey)
        (setq x 0)
        (while (< x ex)
          (cpick-bitmap-set-pixel-linear bitmap offset
                                         (+ (* y width) x)
                                         cpick-syntax-background
                                         cpick-syntax-background
                                         cpick-syntax-background)
          (cl-incf x))
        (cl-incf y)))))

;;; Mouse tracking

(defun cpick-canvas-down-mouse-1 (ev)
  (interactive "@e")
  (with-slots (drawable snapshot) cpick-canvas
    (let* ((position (event-start ev))
           (x-y (posn-object-x-y position))
           (x (car x-y))
           (y (cdr x-y)))
      (cond
       ;; Box
       ((cpick-cursor-inside-box-p x y)
        (cpick-track-mouse-1 cpick-canvas ev
                             #'cpick-draw-box-cursor
                             (lambda ()
                               (cpick-draw-alpha-bar drawable snapshot))))
       ;; Hue bar
       ((cpick-cursor-inside-hue-bar-p x y)
        (cpick-track-mouse-1 cpick-canvas ev
                             #'cpick-draw-hue-bar-cursor
                             (lambda ()
                               (cpick-draw-box drawable snapshot)
                               (cpick-draw-alpha-bar drawable snapshot))))
       ;; Alpha bar
       ((cpick-cursor-inside-alpha-bar-p x y)
        (cpick-track-mouse-1 cpick-canvas ev
                             #'cpick-draw-alpha-bar-cursor))

       ;; Syntax toggle
       ((< cpick-syntax-toggle-y0 y)
        (cond
         ;; RGB
         ((<= cpick-syntax-rgb-x0 x cpick-syntax-rgb-x1)
          (cpick-change-syntax 'cpick-syntax-rgb))
         ;; HEX
         ((<= cpick-syntax-hex-x0 x cpick-syntax-hex-x1)
          (cpick-change-syntax 'cpick-syntax-hex2))
         ;; HSL
         ((<= cpick-syntax-hsl-x0 x cpick-syntax-hsl-x1)
          (cpick-change-syntax 'cpick-syntax-hsl))
         ;; HSV
         ((<= cpick-syntax-hsv-x0 x cpick-syntax-hsv-x1)
          (cpick-change-syntax 'cpick-syntax-hsv))
         ;; VEC
         ((<= cpick-syntax-vec-x0 x cpick-syntax-vec-x1)
          (cpick-change-syntax 'cpick-syntax-vec))))))))

;;; Minor mode

(defvar cpick-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'cpick-frame-delete)
    map)
  "Keymap for cpick minor mode.")

(define-minor-mode cpick-minor-mode
  "Color Picker"
  nil " cpick" cpick-minor-mode-map)

;;; Major mode

(defvar cpick-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'cpick-canvas-down-mouse-1)
    (define-key map "q" 'cpick-frame-delete)
    map)
  "Keymap for cpick major mode.")

(defun cpick-canvas-initialize ()
  (cpick-cursor-box-read-xpm cpick-cursor-box-xpm)
  (with-slots (drawable) cpick-canvas
    (cpick-drawable-clear drawable cpick-background)
    (cpick-draw-checkered-pattern drawable)
    (let* ((hsv (cpick-rgba-vec256-to-hsv cpick-old-color-rgb))
           (h (aref hsv 0))
           (s (aref hsv 1))
           (v (aref hsv 2))
           (rgb (cpick-hsv-to-rgb h 1.0 1.0))
           (alpha (if (cpick-color-vec-alpha-p cpick-old-color-rgb)
                      (aref cpick-old-color-rgb 3)
                    1.0)))
      (aset cpick-hue-color 0 (cpick-rgb-ftoi (aref rgb 0)))
      (aset cpick-hue-color 1 (cpick-rgb-ftoi (aref rgb 1)))
      (aset cpick-hue-color 2 (cpick-rgb-ftoi (aref rgb 2)))
      (cpick-draw-current-area)
      (cpick-draw-box drawable)
      (cpick-draw-hue-bar drawable)
      (cpick-draw-alpha-bar drawable)
      (cpick-draw-syntax-toggle-area drawable)
      (cpick-snapshot cpick-canvas)
      (cpick-cursor-box-draw drawable nil
                             (+ (cpick-rgb-ftoi s) cpick-box-x0)
                             (+ (cpick-rgb-ftoi (- 1.0 v)) cpick-box-y0))
      (cpick-cursor-alpha-bar-draw drawable nil (+ cpick-box-y0 (cpick-rgb-ftoi (- 1.0 alpha))))
      (cpick-cursor-hue-bar-draw drawable nil (+ cpick-box-y0 (cpick-rgb-ftoi (- 1.0 (/ h 360.0)))))
      (cpick-change-syntax))))

(define-derived-mode cpick-mode nil "Color Picker"
  (unless (posframe-workable-p)
    (error "Color Picker: Display does not support images"))
  (cpick-mode-setup cpick-frame-width cpick-frame-height)
  (cpick-canvas-initialize))

(defun cpick-popup-at-point ()
  (interactive)
  (when (cpick-buffer-search-color)
    (unless (posframe-workable-p)
      (error "Color Picker: Display does not support images"))
    (setq cpick-parent-buffer (current-buffer))
    (cpick-minor-mode 1)
    (let* ((pos (window-absolute-pixel-position))
           (btm (save-excursion
                  (progn
                    (forward-line)
                    (window-absolute-pixel-position))))
           (height (- (cdr btm) (cdr pos))))
      (cl-incf (car pos) (/ (- (default-font-width) cpick-frame-width) 2))
      (cl-incf (cdr pos) height)
      (cpick-frame-create)
      (with-selected-frame cpick-frame-base
        (with-current-buffer cpick-buffer-base
          (cpick-mode)
          (let* ((geometry (assq 'geometry (nth 0 (display-monitor-attributes-list))))
                 (resolution-x (nth 3 geometry))
                 (resolution-y (nth 4 geometry)))
            (set-frame-position (selected-frame)
                                (cpick-clamp (car pos) 0 resolution-x)
                                (cpick-clamp (cdr pos) 0 resolution-y)))
          (cpick-frame-show-all))))))

(provide 'color-picker)
;;; color-picker.el ends here
