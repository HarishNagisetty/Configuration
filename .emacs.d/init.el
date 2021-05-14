(require 'package)
(add-to-list 'package-archives 
    '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-babel-load-languages '((emacs-lisp . t) (python . t) (perl . t)))
 '(origami-show-fold-header t)
 '(package-selected-packages
   '(org-bullets undo-fu whitespace-cleanup-mode key-chord evil))
 '(safe-local-variable-values '((origami-fold-style . triple-braces))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:extend t :background "medium spring green" :distant-foreground "gtk_selection_fg_color")))))

;; undo-fu
(require 'undo-fu)

;; evil
(setq evil-shift-width 4)
(setq evil-undo-system 'undo-fu)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-select-search-module 'evil-search-module 'evil-search)
(evil-mode 1)

;; key-chord
(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; whitespace
(require 'whitespace)
(setq whitespace-style '(tabs tab-mark))
(global-whitespace-mode 1)

;; org mode
(setq org-log-done 'time)
(require 'org)

;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; origami
(add-to-list 'load-path "~/.emacs.d/origami.el/")
(require 'origami)

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(global-set-key (kbd "C-x r b")
                (lambda ()
                  (interactive)
                  (bookmark-jump
                   (ido-completing-read "Jump to bookmark: "
                                        (bookmark-all-names)))))

;; verilog mode
(setq verilog-indent-level-directive 2)
(setq verilog-indent-level-behavioral 2)
(setq verilog-indent-level-declaration 2)
(setq verilog-indent-level-module 2)
(setq verilog-indent-level 2)
(setq verilog-indent-lists nil)
(setq verilog-auto-endcomments nil)

;; General Settings

(setq desktop-restore-frames nil)
(desktop-save-mode t)

(setq inhibit-splash-screen t)

(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode t)
  (global-linum-mode))

(show-paren-mode t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq epa-armor t)

(defun hn-font-exists-p (font)
  "Check if font exists"
  (if (null (x-list-fonts font)) nil t))

(if (hn-font-exists-p "Source Code Pro")
    (set-face-attribute 'default nil :font "Source Code Pro" :height 114)
  (set-face-attribute 'default nil :height 124))

(defun hn-set-comment-char (char)
  "Set comment char for current buffer."
  (interactive "sComment char: ")
  (setq comment-start char)
  (font-lock-add-keywords nil `((,(concat comment-start ".+") . font-lock-comment-face))))

(defun hn-origami-fold-style-triple-braces ()
  "Set origami-fold-style to triple-braces."
  (interactive)
  (set (make-local-variable 'origami-fold-style) 'triple-braces))

(defun hn-org-archive-done-tasks ()
  "Archive all DONE tasks in Org buffer"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" nil))
