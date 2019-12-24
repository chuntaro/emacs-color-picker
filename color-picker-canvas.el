;;; color-picker-canvas.el --- Simple canvas for Color Picker -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; Keywords: canvas, mouse, tools

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

;; This file is a simple canvas for `color-picker.el'.

;;; Code:

(require 'eieio)

(defvar cpick-canvas nil)

(defun cpick-mode-setup (width height)
  (unless cpick-canvas
    (setq cpick-canvas (make-instance 'cpick-canvas-class :width width :height height)))
  (buffer-disable-undo)
  (setq buffer-read-only t
        view-read-only nil)
  (setq-local auto-hscroll-mode nil))

(cl-defstruct (cpick-drawable (:conc-name cpick--))
  bitmap
  offset
  width
  height)

(defmacro cpick-with-drawable (spec-list object &rest body)
  (declare (indent 2))
  `(let ,(mapcar (lambda (entry)
                   (if (assq entry (cl-struct-slot-info 'cpick-drawable))
                       (list entry `(,(intern (concat "cpick--" (symbol-name entry))) ,object))
                     (warn "cpick-with-drawable: Unknown slot-name: `%s'" entry)))
                 spec-list)
     ,@body))

(defun cpick-drawable-create (width height)
  (let ((drawable (make-cpick-drawable))
        (header (concat "P6\n"
                        (number-to-string width)
                        " "
                        (number-to-string height)
                        "\n"
                        "255\n")))
    (setf (cpick--bitmap drawable) (encode-coding-string
                                    (concat header (make-string (* width height 3) 255))
                                    'no-conversion)
          (cpick--offset drawable) (length header)
          (cpick--width drawable) width
          (cpick--height drawable) height)
    drawable))

(defclass cpick-canvas-class ()
  ((drawable :initarg :drawable :accessor drawable :type cpick-drawable)
   (image    :initarg :image    :accessor image    :type list)
   (snapshot :initarg :shapshot :accessor snapshot :type string)))

(cl-defmethod initialize-instance ((this cpick-canvas-class) &optional args)
  (cl-call-next-method this)
  (let ((width (plist-get args :width))
        (height (plist-get args :height)))
    (setf (drawable this) (cpick-drawable-create (or width 640) (or height 480))))
  (cpick-create-image this))

(defun cpick-drawable-create-image (drawable)
  (create-image (cpick--bitmap drawable) 'pbm t
                :width (cpick--width drawable)
                :height (cpick--height drawable)
                ;; text (or nil), arrow, hand, vdrag, hdrag, modeline, hourglass
                :pointer 'arrow))

(cl-defmethod cpick-put-image ((this cpick-canvas-class) &key point)
  (remove-images (point-min) (point-max))
  (put-image (image this) (or point (point-min)))
  (image-flush (image this)))

(defun cpick-image-get-bitmap (image)
  (plist-get (cdr image) :data))

(cl-defmethod cpick-snapshot ((this cpick-canvas-class))
  (setf (snapshot this) (copy-sequence (cpick-image-get-bitmap (image this)))))

(cl-defmethod cpick-create-image ((this cpick-canvas-class))
  (setf (image this) (cpick-drawable-create-image (drawable this)))
  (cpick-put-image this)
  (cpick-snapshot this))

(cl-defmethod cpick-force-window-update ((this cpick-canvas-class))
  (image-flush (image this))
  (force-window-update (get-buffer-window)))

(cl-defmethod cpick-track-mouse-1 ((this cpick-canvas-class) ev draw-func &optional up-mouse-1)
  (let* ((drawable (drawable this))
         (image (image this))
         (position (event-start ev))
         (x-y (posn-object-x-y position))
         win obj x1 y1 x y)
    (track-mouse
      (while (or (mouse-movement-p ev)
                 (member 'down (event-modifiers ev)))
        (setq position (event-start ev)
              win (posn-window position)
              obj (posn-object position)
              x-y (posn-object-x-y position)
              x1 (car x-y)
              y1 (cdr x-y)
              x-y (posn-x-y position)
              x (car x-y)
              y (car x-y))
        (when (and (eq (get-buffer-window) win)
                   (eq image obj)
                   (/= 0 x)
                   (/= 0 y))
          (funcall draw-func drawable (snapshot this) x1 y1)
          (cpick-force-window-update this))
        (setq ev (read-event))))
    (when up-mouse-1
      (funcall up-mouse-1))))

(defsubst cpick-bitmap-get-pixel-linear (bitmap offset i alpha)
  (let* ((r (+ offset (* i 3)))
         (g (1+ r)))
    (vector (aref bitmap r)
            (aref bitmap g)
            (aref bitmap (1+ g))
            alpha)))

(defsubst cpick-bitmap-set-pixel-linear (bitmap offset i r g b)
  (let* ((ri (+ offset (* i 3)))
         (gi (1+ ri)))
    (aset bitmap ri r)
    (aset bitmap gi g)
    (aset bitmap (1+ gi) b)))

(defsubst cpick-drawable-clear (drawable gray-scale)
  (cpick-with-drawable (bitmap offset) drawable
    (let ((header (substring-no-properties bitmap 0 offset)))
      (fillarray bitmap gray-scale)
      (dotimes (i offset)
        (aset bitmap i (aref header i))))))

(defsubst cpick-drawable-get-color (drawable x y alpha)
  (cpick-with-drawable (bitmap offset width) drawable
    (let ((i (+ x (* y width))))
      (cpick-bitmap-get-pixel-linear bitmap offset i alpha))))

(defun cpick-drawable-restore-rect (drawable snapshot x0 y0 x1 y1)
  (cpick-with-drawable (bitmap offset width) drawable
    (while (< y0 y1)
      (let ((x x0)
            (offx (* y0 width)))
        (while (< x x1)
          (let* ((i (+ x offx))
                 (r (+ offset (* i 3)))
                 (g (1+ r))
                 (b (1+ g)))
            (aset bitmap r (aref snapshot r))
            (aset bitmap g (aref snapshot g))
            (aset bitmap b (aref snapshot b)))
          (cl-incf x)))
      (cl-incf y0))))

(provide 'color-picker-canvas)
;;; color-picker-canvas.el ends here
