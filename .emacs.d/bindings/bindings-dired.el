;; -*- lexical-binding: t -*-

(setq /bindings/dired/localleader-map (make-sparse-keymap))

(defun /bindings/dired/up-directory ()
  "Go to parent directory and keep some buffer properties."
  (interactive)
  (let ((is-sidebar (bound-and-true-p /bindings/core/dired-sidebar)))
    (dired-up-directory)
    (when is-sidebar (/bindings/core/dired-sidebar-properties))))

(/core/boot/after '/bindings/core
  (/bindings/core/define-keys /bindings/dired/localleader-map
    ("u" #'/bindings/dired/up-directory "Up Directory")))

;; Add localleader-map to dired-mode-map
(/core/boot/after [/bindings/core dired]
  (/bindings/core/define-keys dired-mode-map
    (/bindings/core/localleader /bindings/dired/localleader-map)))

;; Add some evil bindings for navigation
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map
              (kbd "w") 'evil-forward-word-begin)
            (define-key evil-normal-state-local-map
              (kbd "gg") 'evil-goto-first-line)
            (define-key evil-normal-state-local-map
              (kbd "G") 'evil-goto-line)))

(provide '/bindings/dired)
