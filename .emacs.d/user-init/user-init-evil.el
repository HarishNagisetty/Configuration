;;; user-init-evil.el

(setq evil-shift-width 4)
(setq evil-undo-system 'undo-fu)
(setq evil-want-C-u-scroll t)

(require 'undo-fu)
(require 'evil)

(evil-select-search-module 'evil-search-module 'evil-search)
(evil-mode 1)

(provide 'user-init-evil)
