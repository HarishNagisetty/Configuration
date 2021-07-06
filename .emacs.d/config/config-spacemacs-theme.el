;; -*- lexical-binding: t -*-

(/core/boot/require-package 'spacemacs-theme)

(setq spacemacs-theme-org-agenda-height nil)
(setq spacemacs-theme-org-height nil)

(defun /config/spacemacs-theme/dark ()
  "Load Spacemacs Dark theme."
  (interactive)
  (load-theme 'spacemacs-dark t))

(defun /config/spacemacs-theme/light ()
  "Load Spacemacs Light theme."
  (interactive)
  (load-theme 'spacemacs-light t))

(provide '/config/spacemacs-theme)
