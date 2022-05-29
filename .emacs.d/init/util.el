;; -*- lexical-binding: t -*-

(defun /init/util/font-exists-p (font)
  "Return non-nil if FONT is available."
  (if (null (x-list-fonts font)) nil t))

(defun /init/util/font-monospace ()
  "Set the default font to a monospace font." 
  (interactive)
  (when (display-graphic-p)
    (cond
     ((/init/util/font-exists-p "Source Code Pro")
      (set-face-attribute 'default nil :font "Source Code Pro" :height 100))
     ((/init/util/font-exists-p "Monospace")
      (set-face-attribute 'default nil :font "Monospace" :height 120))
     (t (set-face-attribute 'default nil :height 114)))))

(defun /init/util/position-frame ()
  "Resize and position frame to top left."
  (interactive)
  (set-frame-width (selected-frame) 110)
  (set-frame-height (selected-frame) 38)
  (set-frame-position (selected-frame) 0 0))

(defun /init/util/set-comment-char (char)
  "Set the comment character for current buffer."
  (interactive "sComment Character: ")
  (setq comment-start char)
  (font-lock-add-keywords
   nil `((,(concat comment-start ".+") . font-lock-comment-face))))

(defun /init/util/toggle-line-numbers ()
  "Toggle line numbering for the current buffer."
  (interactive)
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 'toggle) 
    (linum-mode 'toggle)))

(defun /init/util/write-backup ()
  "Write current buffer to a timestamped backup file."
  (interactive)
  (if buffer-file-name
      (write-region
       (point-min) (point-max)
       (concat buffer-file-name "."
               (substring (shell-command-to-string "date +%s") 0 -1)))
    (message "Current buffer does not have a file name.")))

(defun /init/util/insert-date ()
  "Insert the date and time into the current buffer."
  (interactive)
  (insert (shell-command-to-string "echo -n `date`")))

(defun /init/util/disable-themes ()
  "Disable enabled themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun /init/util/personal-light-theme ()
  "Load Personal Light theme."
  (interactive)
  (/init/util/disable-themes)
  (load-theme 'personal-light t))

(defun /init/util/spacemacs-dark-theme ()
  "Load Spacemacs Dark theme."
  (interactive)
  (require 'spacemacs-common)
  (/init/util/disable-themes)
  (load-theme 'spacemacs-dark t))

(defun /init/util/spacemacs-light-theme ()
  "Load Spacemacs Light theme."
  (interactive)
  (require 'spacemacs-common)
  (/init/util/disable-themes)
  (load-theme 'spacemacs-light t))

(defun /init/util/require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun /init/util/open-occur-by-major-mode ()
  (interactive)
  (let* ((verilog-pattern "task\\|function\\|class\\|module")
         (regexp-alist
          `((python-mode . "class\\|def")
            (emacs-lisp-mode . "(def")
            (verilog-mode . ,verilog-pattern)
            (verilog3-mode . ,verilog-pattern)))
         active)
    (setq active (assoc major-mode regexp-alist))
    (when active
      (occur (cdr active))
      (other-window 1))))
