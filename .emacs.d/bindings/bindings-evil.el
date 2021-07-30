;; -*- lexical-binding: t -*-

;; Use nearly all emacs bindings in insert state
(/core/boot/after 'evil
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state))

(provide '/bindings/evil)
