;;; user-init-global.el --- Global configuration of installed packages

;; dired
; Show sizes in human readable form
; Don't show user or group
(setq dired-listing-switches "-alhgG")
(add-hook 'dired-mode-hook 'hn-no-line-numbers)
; Evil uses most dired mappings by default.
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map
              (kbd (concat hn-evil-localleader "u")) 'hn-dired-up-directory)
            (evil-local-set-key 'normal (kbd "w") 'evil-forward-word-begin)
            (evil-local-set-key 'normal (kbd "gg") 'evil-goto-first-line)
            (evil-local-set-key 'normal (kbd "G") 'evil-goto-line)))

;; Buffer List
; After selecting buffer through the Buffer List, I don't want evil
; alternate file to be *Buffer List*
(advice-add 'Buffer-menu-this-window :after
            (lambda () (kill-buffer "*Buffer List*")))

;; Global Key Bindings

(eval-after-load 'evil
  (lambda ()
    (which-key-add-keymap-based-replacements evil-normal-state-map
      (concat hn-evil-leader "b") '("Switch Buffer")
      (concat hn-evil-leader "o") '("Open...")
      (concat hn-evil-leader "q") '("Delete Buffer")
      (concat hn-evil-leader "w") '("Window...")
      )

    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "b")) 'ido-switch-buffer)
    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "ot")) 'hn-dired-open-sidebar)
    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "q")) (lambda ()
                                          (interactive)
                                          (kill-this-buffer)
                                          (when (> (count-windows) 1)
                                            (delete-window))))
    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "wc")) 'evil-window-delete)
    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "wh")) 'evil-window-left)
    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "wj")) 'evil-window-down)
    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "wk")) 'evil-window-up)
    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "wl")) 'evil-window-right)
    (define-key evil-normal-state-map
      (kbd (concat hn-evil-leader "wo")) 'delete-other-windows)
    ))

(provide 'user-init-global)
