;; -*- lexical-binding: t -*-

(setq /bindings/core/leader ",")
(setq /bindings/core/localleader "SPC")
(setq /bindings/core/global-leader-map (make-sparse-keymap))

(defmacro /bindings/core/define-keys (keymap &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect
                `(let ((seq ,(car binding))
                       (func ,(cadr binding))
                       (desc ,(car (cddr binding))))
                   (when seq
                     (define-key ,keymap (kbd seq) func))
                   (when desc
                     (which-key-add-keymap-based-replacements
                       ,keymap seq desc))))))

;;; Mapped Functions

(defun /bindings/core/delete-buffer ()
  (interactive)
  (kill-this-buffer)
  (when (> (count-windows) 1)
    (delete-window)))

(defun /bindings/core/dired-sidebar-properties ()
  "Set special properties when using dired like a sidebar."
  (setq-local /bindings/core/dired-sidebar t)
  (dired-hide-details-mode)
  (define-key evil-normal-state-local-map
    (kbd "<return>") '/bindings/core/open-dired-file))

(defun /bindings/core/open-dired-file ()
  "Open in same window if directory, other window if file"
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (is-sidebar (bound-and-true-p /bindings/core/dired-sidebar)))
    (if (file-directory-p file)
        (progn
          (dired-find-file)
          (when is-sidebar (/bindings/core/dired-sidebar-properties)))
      (dired-find-file-other-window)
      (other-window -1))))

(defun /bindings/core/open-dired ()
  "Open dired like a sidebar"
  (interactive)
  (let ((width (round (* 0.33 (window-width)))))
    (split-window-horizontally width)
    (dired default-directory)
    (/bindings/core/dired-sidebar-properties)))

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

(/bindings/core/define-keys /bindings/core/global-leader-map
  ("b" #'ido-switch-buffer "Switch Buffer")
  ("o" nil "Open...")
  ("ot" #'/bindings/core/open-dired "Open Dired")
  ("om" #'/bindings/core/open-marks "Open Bookmarks")
  ("on" #'/bindings/core/open-notes "Open Notes")
  ("os" #'/bindings/core/open-scratch-buffer "Open Scratch Buffer")
  ("p" nil "Preferences...")
  ("pw" 'toggle-truncate-lines "Toggle Line-Wrap")
  ("q" #'/bindings/core/delete-buffer "Delete Buffer")
  ("w" nil "Windows...")
  ("wc" #'evil-window-delete "Delete Window")
  ("wh" #'evil-window-left "Move Left")
  ("wj" #'evil-window-down "Move Down")
  ("wk" #'evil-window-up "Move Up")
  ("wl" #'evil-window-right "Move Right")
  ("wo" #'delete-other-windows "Delete Other Windows"))

(/core/boot/after 'evil
  (/bindings/core/define-keys evil-motion-state-map
    (/bindings/core/leader /bindings/core/global-leader-map "Leader")))

(/bindings/core/define-keys global-map
  ("C-c c" #'company-complete "Company Complete")
  ("C-c l" /bindings/core/global-leader-map "Leader"))

(provide '/bindings/core)
