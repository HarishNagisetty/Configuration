;; -*- lexical-binding: t -*-

(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq epa-armor t)
(setq scroll-conservatively 101)
(setq desktop-restore-frames nil)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)

(add-hook 'text-mode-hook 'auto-fill-mode)

(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))
(column-number-mode 1)
(show-paren-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; Save minibuffer history.
(savehist-mode 1)
;; Save desktop session.
(desktop-save-mode 1)

(/core/boot/after '/config/util
  (/config/util/harish-theme))

;(require 'hide-mode-line)
;(global-hide-mode-line-mode)

;;; Fonts
(defun /config/core/font-exists-p (font)
  "Check if font is available."
  (if (null (x-list-fonts font)) nil t))
(when (display-graphic-p)
  (cond
   ((/config/core/font-exists-p "Source Code Pro")
    (set-face-attribute 'default nil :font "Source Code Pro" :height 114))
   ((/config/core/font-exists-p "Monospace")
    (set-face-attribute 'default nil :font "Monospace" :height 140))
   (t (set-face-attribute 'default nil :height 114))))

;;; Buffer List
;; After selecting buffer through the Buffer List, I don't want evil
;; alternate file to be *Buffer List*
(advice-add 'Buffer-menu-this-window :after
            (lambda () (kill-buffer "*Buffer List*")))

(provide '/config/core)
