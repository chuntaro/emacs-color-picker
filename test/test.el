;;; -*- lexical-binding: t; -*-

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'tango-dark)

(package-initialize)

(add-to-list 'load-path ".")
(require 'color-picker)

(global-set-key [mouse-1] 'cpick-popup-at-point)
(global-set-key "\C-cp" 'cpick-popup-at-point)

(find-file "test/test.css")
