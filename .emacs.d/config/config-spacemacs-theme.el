;; -*- lexical-binding: t -*-

(/core/boot/require-package 'spacemacs-theme)

(defun /config/spacemacs-theme/dark ()
  "Load Spacemacs Dark theme"
  (interactive)
  (load-theme 'spacemacs-dark t))

(defun /config/spacemacs-theme/light ()
  "Load Spacemacs Light theme"
  (interactive)
  (load-theme 'spacemacs-light t))

(/config/spacemacs-theme/dark)

(provide '/config/spacemacs-theme)
