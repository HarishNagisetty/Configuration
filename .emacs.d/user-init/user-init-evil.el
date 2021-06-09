;;; user-init-evil.el

(setq evil-shift-width 4)
(setq evil-undo-system 'undo-fu)
(setq evil-want-C-u-scroll t)

(require 'undo-fu)
(require 'evil)

(evil-select-search-module 'evil-search-module 'evil-search)
(evil-mode 1)

;; Global Key Bindings
(setq hn-evil-leader ",")

(define-key evil-normal-state-map
  (kbd (concat hn-evil-leader "b")) 'ido-switch-buffer)
(define-key evil-normal-state-map
  (kbd (concat hn-evil-leader "q")) (lambda ()
                                      (interactive)
                                      (kill-this-buffer)
                                      (when (> (count-windows) 1)
                                        (delete-window))))
(define-key evil-normal-state-map
  (kbd (concat hn-evil-leader "ot")) 'hn-open-dired-sidebar)

(provide 'user-init-evil)
