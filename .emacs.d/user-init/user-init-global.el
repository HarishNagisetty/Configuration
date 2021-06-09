;;; user-init-global.el --- Global configuration of installed packages

;; dired
(add-hook 'dired-mode-hook 'hn-no-line-numbers)
; Evil uses most dired mappings by default.
(add-hook 'dired-mode-hook
          (lambda ()
            (evil-local-set-key 'normal (kbd "w") 'evil-forward-word-begin)
            (evil-local-set-key 'normal (kbd "gg") 'evil-goto-first-line)
            (evil-local-set-key 'normal (kbd "G") 'evil-goto-line)))

(provide 'user-init-global)
