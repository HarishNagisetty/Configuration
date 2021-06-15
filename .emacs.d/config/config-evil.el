;; -*- lexical-binding: t -*-

(setq evil-shift-width 4)
(setq evil-undo-system 'undo-fu)
(setq evil-want-C-u-scroll t)

(/core/boot/require-package 'undo-fu)
(/core/boot/require-package 'evil)
(require 'undo-fu)
(require 'evil)

(evil-select-search-module 'evil-search-module 'evil-search)
(evil-mode 1)

(provide '/config/evil)
