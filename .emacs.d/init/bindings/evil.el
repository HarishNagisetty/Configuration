;; -*- lexical-binding: t -*-

;; Use vanilla emacs bindings in the insert mode.
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;; jk to return to normal state.
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
