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
      (set-face-attribute 'default nil :font "Monospace" :height 140))
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

(defun /config/util/insert-date ()
  "Insert the date and time into the current buffer."
  (interactive)
  (insert (shell-command-to-string "echo -n `date`")))
