;; -*- lexical-binding: t -*-

;; Start directly with the scratch buffer.
(setq inhibit-splash-screen t)

;; Don't make backup (~) files.
(setq make-backup-files nil)

;; Save minibuffer history.
(savehist-mode 1)

;(setq desktop-restore-frames nil)
;(desktop-save-mode t)

;; Show line numbers.
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))

;; Show column numbers.
(column-number-mode t)

;; Show matching parenthesis.
(show-paren-mode t)

;; Indentation shouldn't use tabs.
(setq-default indent-tabs-mode nil)

;; ASCII armored output.
(setq epa-armor t)

;; Set default font.
(defun /config/core/font-exists-p (font)
  "Check if font is available."
  (if (null (x-list-fonts font)) nil t))

(if (/config/core/font-exists-p "Source Code Pro")
    (set-face-attribute 'default nil :font "Source Code Pro" :height 114)
  (set-face-attribute 'default nil :height 124))

(provide '/config/core)
