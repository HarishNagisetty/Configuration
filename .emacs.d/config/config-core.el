;; -*- lexical-binding: t -*-

(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq epa-armor t)
(setq dired-listing-switches "-alhgG")

(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))
(setq-default indent-tabs-mode nil)
(column-number-mode t)
(show-paren-mode t)

;;; History
;; Save minibuffer history.
(savehist-mode 1)
;; Save desktop session.
;(setq desktop-restore-frames nil)
;(desktop-save-mode t)

;;; Fonts
(defun /config/core/font-exists-p (font)
  "Check if font is available."
  (if (null (x-list-fonts font)) nil t))
(if (/config/core/font-exists-p "Source Code Pro")
    (set-face-attribute 'default nil :font "Source Code Pro" :height 114)
  (set-face-attribute 'default nil :height 124))

;;; Buffer List
;; After selecting buffer through the Buffer List, I don't want evil
;; alternate file to be *Buffer List*
(advice-add 'Buffer-menu-this-window :after
            (lambda () (kill-buffer "*Buffer List*")))

(provide '/config/core)
