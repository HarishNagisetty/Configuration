;; -*- lexical-binding: t -*-

(setq org-startup-folded t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-log-done 'note)
(setq org-cycle-open-archived-trees nil)

;(/core/boot/require-package 'org-bullets)
;(add-hook 'org-mode-hook 'org-bullets-mode)

(provide '/config/org)
