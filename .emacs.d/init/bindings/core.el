;; -*- lexical-binding: t -*-

(setq /init/bindings/core/leader ",")
(setq /init/bindings/core/localleader "SPC")
(setq /init/bindings/core/global-leader-map (make-sparse-keymap))

(defmacro /init/bindings/core/define-keys (keymap &rest body)
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

(defun /init/bindings/core/delete-buffer ()
  (interactive)
  (kill-this-buffer)
  (when (> (count-windows) 1)
    (delete-window)))

(defun /init/bindings/core/set-dired-sidebar-properties ()
  "Set special properties when using dired like a sidebar."
  (setq-local /init/bindings/core/is-dired-sidebar t)
  (dired-hide-details-mode)
  (define-key evil-normal-state-local-map
    (kbd "<return>") '/init/bindings/core/open-dired-file-directory))

(defun /init/bindings/core/open-dired-file-directory ()
  "Open in same window if directory, other window if file"
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (is-sidebar (bound-and-true-p /init/bindings/core/is-dired-sidebar)))
    (if (file-directory-p file)
        (progn
          (dired-find-file)
          (when is-sidebar (/init/bindings/core/set-dired-sidebar-properties)))
      (dired-find-file-other-window)
      (other-window -1))))

(defun /init/bindings/core/open-dired-as-sidebar ()
  "Open dired like a sidebar"
  (interactive)
  (let ((width (round (* 0.25 (window-width)))))
    (split-window-horizontally width)
    (dired default-directory)
    (/init/bindings/core/set-dired-sidebar-properties)))

(defun /init/bindings/core/open-marks ()
  (interactive)
  (split-window-vertically)
  (find-file "~/.marks")
  (markdown-mode))

(defun /init/bindings/core/open-notes ()
  (interactive)
  (split-window-vertically)
  (find-file "~/.notes")
  (/config/util/set-comment-char "#"))

(defun /init/bindings/core/open-scratch-buffer nil
  "Open/Create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;;; Leader Mappings
;; Use only lowercase letters
(/init/bindings/core/define-keys /init/bindings/core/global-leader-map
  ("b" #'ido-switch-buffer "Switch Buffer")
  ("d" #'/init/util/insert-date "Insert Date")
  ("f" #'ido-find-file "Find File")
  ("o" nil "Open...")
  ("ot" #'/init/bindings/core/open-dired-as-sidebar "Open Dired")
  ("om" #'/init/bindings/core/open-marks "Open Bookmarks")
  ("on" #'/init/bindings/core/open-notes "Open Notes")
  ("os" #'/init/bindings/core/open-scratch-buffer "Open Scratch Buffer")
  ("oo" #'/init/util/open-occur-by-major-mode "Open Occur")
  ("p" nil "Preferences...")
  ("p-" #'text-scale-decrease "Decrease Text Scale")
  ("p+" #'text-scale-increase "Increase Text Scale")
  ("p0" (lambda () (interactive) (text-scale-mode 0)) "Reset Text Scale")
  ("pf" #'toggle-frame-fullscreen "Toggle Frame Fullscreen")
  ("pm" #'menu-bar-mode "Toggle Menu Bar")
  ("ps" #'flyspell-mode "Toggle Spell Check On/Off")
  ("pt" nil "Change Theme")
  ("pte" #'/init/util/eink-theme "E-Ink Theme")
  ("pth" #'/init/util/personal-light-theme "Personal Light Theme")
  ("ptn" #'/init/util/disable-themes "No Theme")
  ("ptb" nil "Bespoke Theme")
  ("ptbl" #'bespoke/light-theme "Bespoke Light Theme")
  ("ptbd" #'bespoke/dark-theme "Bespoke Dark Theme")
  ("pts" nil "Spacemacs Theme")
  ("ptsl" #'/init/util/spacemacs-light-theme "Spacemacs Light Theme")
  ("ptsd" #'/init/util/spacemacs-dark-theme"Spacemacs Dark Theme")
  ("pw" 'toggle-truncate-lines "Toggle Line-Wrap")
  ("q" #'/init/bindings/core/delete-buffer "Delete Buffer")
  ("w" nil "Windows...")
  ("wc" #'evil-window-delete "Delete Window")
  ("wh" #'evil-window-left "Move Left")
  ("wj" #'evil-window-down "Move Down")
  ("wk" #'evil-window-up "Move Up")
  ("wl" #'evil-window-right "Move Right")
  ("wo" #'delete-other-windows "Delete Other Windows"))

(setq /init/bindings/core/global-map
      (copy-keymap /init/bindings/core/global-leader-map))

;;; Global Mappings
;; Use only uppercase letters
(/init/bindings/core/define-keys /init/bindings/core/global-map
  ("C" #'company-complete "Company Complete")
  ("G" (lambda ()
         (interactive)
         (message buffer-file-name)) "Display File Name")
  ("N" #'evil-complete-next "Complete Next")
  ("P" #'evil-complete-previous "Complete Previous")
  ("S" #'ispell-word "Correct Spelling at Point")
  ("V" (lambda ()
         (interactive)
         (insert (gui-get-selection)))
   "Paste Contents of Primary Clipboard")
  ("O" #'just-one-space "Just One Space"))

(/init/bindings/core/define-keys evil-motion-state-map
  (/init/bindings/core/leader /init/bindings/core/global-leader-map "Leader"))

(/init/bindings/core/define-keys global-map
  ("C-c" /init/bindings/core/global-map))
