;; -*- lexical-binding: t -*-

(setq /bindings/core/leader ",")
(setq /bindings/core/localleader "SPC")
(setq /bindings/core/global-leader-map (make-sparse-keymap))

(defmacro /bindings/core/define-prefix-keys (keymap prefix &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect
                `(let ((seq ,(car binding))
                       (func ,(cadr binding))
                       (desc ,(caddr binding)))
                   (when seq
                     (define-key ,keymap (kbd seq) func))
                   (when desc
                     (which-key-add-key-based-replacements
                       (if ,prefix
                           (concat ,prefix " " seq)
                         seq)
                       desc))))))

(defmacro /bindings/core/define-leader-keys (keymap &rest body)
  (declare (indent defun))
  `(/bindings/core/define-prefix-keys
     ,keymap /bindings/core/leader ,@body))

(defmacro /bindings/core/define-localleader-keys (keymap &rest body)
  (declare (indent defun))
  `(/bindings/core/define-prefix-keys
     ,keymap /bindings/core/localleader ,@body))


;;; Mapped Functions

(defun /bindings/core/delete-buffer ()
  (interactive)
  (kill-this-buffer)
  (when (> (count-windows) 1)
    (delete-window)))

(defun /bindings/core/open-dired-file ()
  "Open in same window if directory, other window if file"
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (hide-details (bound-and-true-p dired-hide-details-mode)))
    (if (file-directory-p file)
        (progn
          (dired-find-file)
          (define-key evil-normal-state-local-map
            (kbd "<return>") '/bindings/core/open-dired-file)
          ;; If hide-details was previously set, do
          ;; the same for this buffer too.
          (when hide-details
            (dired-hide-details-mode)))
      (dired-find-file-other-window)
      (other-window -1))))

(defun /bindings/core/open-dired ()
  "Open dired like a sidebar"
  (interactive)
  (let ((width (round (* 0.33 (window-width)))))
    (split-window-horizontally width)
    (dired default-directory)
    (dired-hide-details-mode)
    (define-key evil-normal-state-local-map
      (kbd "<return>") '/bindings/core/open-dired-file)))

(defun /bindings/core/open-marks ()
  (interactive)
  (split-window-vertically)
  (find-file "~/.marks")
  (markdown-mode))

(defun /bindings/core/open-notes ()
  (interactive)
  (split-window-vertically)
  (find-file "~/.notes")
  (/config/util/set-comment-char "#"))

(defun /bindings/core/open-scratch-buffer nil
  "Open/Create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;;; Global Mappings

(/bindings/core/define-leader-keys /bindings/core/global-leader-map
  ("b" #'ido-switch-buffer "Switch Buffer")
  ("o" nil "Open...")
  ("ot" #'/bindings/core/open-dired "Open Dired")
  ("om" #'/bindings/core/open-marks "Open Bookmarks")
  ("on" #'/bindings/core/open-notes "Open Notes")
  ("os" #'/bindings/core/open-scratch-buffer "Open Scratch Buffer")
  ("q" #'/bindings/core/delete-buffer "Delete Buffer")
  )

(provide '/bindings/core)

;;;; Markdown Mode
;(add-hook 'markdown-mode-hook
;  (lambda ()
;    (define-key evil-normal-state-local-map
;      (kbd "<return>")
;      'hn-follow-markdown-link-on-line)
;    ))
;
;;;; dired
;;; Show sizes in human readable form
;;; Don't show user or group
;(setq dired-listing-switches "-alhgG")
;(add-hook 'dired-mode-hook 'hn-no-line-numbers)
;;; Local bindings
;(eval-after-load 'dired
;  (lambda ()
;    ;; Clear local leader
;    (define-key dired-mode-map (kbd hn-evil-localleader) nil)
;    (define-key dired-mode-map
;      (kbd (concat hn-evil-localleader "u")) 'hn-dired-up-directory)
;    ))
;;; Add some evil bindings for navigation
;(add-hook 'dired-mode-hook
;          (lambda ()
;            (define-key evil-normal-state-local-map
;              (kbd "w") 'evil-forward-word-begin)
;            (define-key evil-normal-state-local-map
;              (kbd "gg") 'evil-goto-first-line)
;            (define-key evil-normal-state-local-map
;              (kbd "G") 'evil-goto-line)))
;
;;;; Buffer List
;;; After selecting buffer through the Buffer List, I don't want evil
;;; alternate file to be *Buffer List*
;(advice-add 'Buffer-menu-this-window :after
;            (lambda () (kill-buffer "*Buffer List*")))
;
;
;;;; Global Key Bindings
;(eval-after-load 'evil
;  (lambda ()
;    ;; Clear leader and local leader for which-key
;    (define-key evil-motion-state-map (kbd hn-evil-leader) nil)
;    (define-key evil-motion-state-map (kbd hn-evil-localleader) nil)
;
;    (define-key evil-motion-state-map
;      (kbd "<tab>") 'evil-toggle-fold)
;
;    (which-key-add-keymap-based-replacements evil-motion-state-map
;      (concat hn-evil-leader "o") '("Open...")
;      (concat hn-evil-leader "om") '("Open Bookmarks")
;      (concat hn-evil-leader "on") '("Open Notes")
;      (concat hn-evil-leader "os") '("Open Scratch")
;      (concat hn-evil-leader "q") '("Delete Buffer")
;      (concat hn-evil-leader "w") '("Window...")
;      )
;
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "ot")) 'hn-dired-open-sidebar)
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "om")) (lambda ()
;                                           (interactive)
;                                           (split-window-vertically)
;                                           (find-file "~/.marks")
;                                           (markdown-mode)))
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "on")) (lambda ()
;                                           (interactive)
;                                           (split-window-vertically)
;                                           (find-file "~/.notes")
;                                           (hn-set-comment-char "#")))
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "os")) 'hn-open-scratch-buffer)
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "q")) (lambda ()
;                                          (interactive)
;                                          (kill-this-buffer)
;                                          (when (> (count-windows) 1)
;                                            (delete-window))))
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "wc")) 'evil-window-delete)
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "wh")) 'evil-window-left)
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "wj")) 'evil-window-down)
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "wk")) 'evil-window-up)
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "wl")) 'evil-window-right)
;    (define-key evil-motion-state-map
;      (kbd (concat hn-evil-leader "wo")) 'delete-other-windows)
;    ))
