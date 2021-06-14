;;; user-init-global.el --- Global configuration of installed packages

;;; Markdown Mode
(eval-after-load 'dired
  (lambda ()
    ;; Clear local leader
    (define-key markdown-mode-map (kbd hn-evil-localleader) nil)
    (define-key markdown-mode-map
      (kbd (concat hn-evil-localleader "<return>")) 'hn-dired-up-directory)
    ))

;;; dired
;; Show sizes in human readable form
;; Don't show user or group
(setq dired-listing-switches "-alhgG")
(add-hook 'dired-mode-hook 'hn-no-line-numbers)
;; Local bindings
(eval-after-load 'dired
  (lambda ()
    ;; Clear local leader
    (define-key dired-mode-map (kbd hn-evil-localleader) nil)
    (define-key dired-mode-map
      (kbd (concat hn-evil-localleader "u")) 'hn-dired-up-directory)
    ))
;; Add some evil bindings for navigation
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map
              (kbd "w") 'evil-forward-word-begin)
            (define-key evil-normal-state-local-map
              (kbd "gg") 'evil-goto-first-line)
            (define-key evil-normal-state-local-map
              (kbd "G") 'evil-goto-line)))

;;; Buffer List
;; After selecting buffer through the Buffer List, I don't want evil
;; alternate file to be *Buffer List*
(advice-add 'Buffer-menu-this-window :after
            (lambda () (kill-buffer "*Buffer List*")))

;;; Global Key Bindings
(eval-after-load 'evil
  (lambda ()
    ;; Clear leader and local leader for which-key
    (define-key evil-motion-state-map (kbd hn-evil-leader) nil)
    (define-key evil-motion-state-map (kbd hn-evil-localleader) nil)

    (which-key-add-keymap-based-replacements evil-motion-state-map
      (concat hn-evil-leader "b") '("Switch Buffer")
      (concat hn-evil-leader "o") '("Open...")
      (concat hn-evil-leader "om") '("Open Bookmarks")
      (concat hn-evil-leader "on") '("Open Notes")
      (concat hn-evil-leader "os") '("Open Scratch")
      (concat hn-evil-leader "q") '("Delete Buffer")
      (concat hn-evil-leader "w") '("Window...")
      )

    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "b")) 'ido-switch-buffer)
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "ot")) 'hn-dired-open-sidebar)
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "om")) (lambda ()
                                           (interactive)
                                           (split-window-vertically)
                                           (find-file "~/.marks")
                                           (markdown-mode)))
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "on")) (lambda ()
                                           (interactive)
                                           (split-window-vertically)
                                           (find-file "~/.notes")
                                           (hn-set-comment-char "#")))
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "os")) 'hn-open-scratch-buffer)
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "q")) (lambda ()
                                          (interactive)
                                          (kill-this-buffer)
                                          (when (> (count-windows) 1)
                                            (delete-window))))
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "wc")) 'evil-window-delete)
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "wh")) 'evil-window-left)
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "wj")) 'evil-window-down)
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "wk")) 'evil-window-up)
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "wl")) 'evil-window-right)
    (define-key evil-motion-state-map
      (kbd (concat hn-evil-leader "wo")) 'delete-other-windows)
    ))

(provide 'user-init-global)
