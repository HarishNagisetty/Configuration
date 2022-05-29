;; -*- lexical-binding: t -*-

;; Start in normal mode.
(evil-set-initial-state 'occur-mode 'normal)

;; Press <return> to goto occurrence.
(evil-define-key 'normal occur-mode-map
  (kbd "<return>") 'occur-mode-goto-occurrence)
