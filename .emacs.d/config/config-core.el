;; -*- lexical-binding: t -*-

(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq epa-armor t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq scroll-conservatively 101)
(setq desktop-restore-frames nil)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq frame-resize-pixelwise t)

(put 'narrow-to-region 'disabled nil)

(add-hook 'text-mode-hook 'auto-fill-mode)

(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))
(column-number-mode 1)
(show-paren-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(savehist-mode 1)
;(desktop-save-mode 1)

;;; Fonts
(/core/boot/after '/config/util (/config/util/font-monospace))

;;; Theme
(/core/boot/after '/config/util (/config/util/harish-theme))

;;; Buffer List
;; After selecting buffer through the Buffer List, I don't want evil
;; alternate file to be *Buffer List*
(advice-add 'Buffer-menu-this-window :after
            (lambda () (kill-buffer "*Buffer List*")))

(provide '/config/core)
