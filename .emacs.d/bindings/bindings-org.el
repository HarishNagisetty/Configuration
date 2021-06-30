;; -*- lexical-binding: t -*-

(setq /bindings/org/localleader-map (make-sparse-keymap))

(defun /bindings/org/org-insert-link-prefix ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-insert-link))

(/core/boot/after '/bindings/core
  (/bindings/core/define-keys /bindings/org/localleader-map
    ("a" #'org-archive-to-archive-sibling "Archive This Entry")
    ("l" nil "Links")
    ("li" #'org-insert-link "Insert Link")
    ("lf" #'/bindings/org/org-insert-link-prefix "Insert File Link")
    ("o" #'org-insert-heading-respect-content "Add Heading")
    ("t" #'org-insert-todo-heading-respect-content "Add TODO Heading")))

;; Add localleader-map to evil-normal-state-local-map
(add-hook 'org-mode-hook
          (lambda ()
            (/core/boot/after '/bindings/core
              (/bindings/core/define-keys evil-normal-state-local-map
                (/bindings/core/localleader
                 /bindings/org/localleader-map
                 "Local Leader")))))

;; Tab to toggle fold instead of cycling default.
(/core/boot/after [/bindings/core org]
  (/bindings/core/define-keys org-mode-map
    ("<tab>" #'outline-toggle-children)))

(provide '/bindings/org)
