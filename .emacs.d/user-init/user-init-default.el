;;; user-init-default.el --- Changes on emacs default values.

(setq inhibit-splash-screen t)

;(setq desktop-restore-frames nil)
;(desktop-save-mode t)

;; Show line numbers
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode t)
  (global-linum-mode))

;; Show matching parenthesis
(show-paren-mode t)

;; Show column numbers
(column-number-mode t)

;; Indentation shouldn't use tabs
(setq-default indent-tabs-mode nil)

;; ASCII armored output
(setq epa-armor t)

;; Set default font
(if (hn-font-exists-p "Source Code Pro")
    (set-face-attribute 'default nil :font "Source Code Pro" :height 114)
  (set-face-attribute 'default nil :height 124))

(provide 'user-init-default)
