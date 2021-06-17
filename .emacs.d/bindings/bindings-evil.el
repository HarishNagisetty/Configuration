;; -*- lexical-binding: t -*-

;; Use nearly all emacs bindings in insert state
(/core/boot/after 'evil
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-p") #'evil-complete-previous)
  (define-key evil-insert-state-map (kbd "C-n") #'evil-complete-next))

(provide '/bindings/evil)
