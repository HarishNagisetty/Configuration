;; -*- lexical-binding: t -*-

(defun /config/util/set-comment-char (char)
  "Set comment character for current buffer."
  (interactive "sComment Character: ")
  (setq comment-start char)
  (font-lock-add-keywords nil `((,(concat comment-start ".+") . font-lock-comment-face))))

(defun /config/util/origami-fold-style-triple-braces ()
  "Set origami-fold-style to triple-braces."
  (interactive)
  (set (make-local-variable 'origami-fold-style) 'triple-braces))

(defun /config/util/org-archive-done-tasks ()
  "Archive all DONE tasks in Org buffer"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" nil))

(defun /config/util/no-line-numbers ()
  "Disable line numbering for the current buffer."
  (interactive)
  (if (fboundp 'global-display-line-numbers-mode)
      (display-line-numbers-mode 0)
    (linum-mode 0)))

(defun /config/util/write-backup ()
  "Write current buffer to a timestamped backup file."
  (interactive)
  (if buffer-file-name
      (write-region
       (point-min) (point-max)
       (concat buffer-file-name "."
               (substring (shell-command-to-string "date +%s") 0 -1)))
    (message "Current buffer does not have a file name.")))

(defun /config/util/disable-themes ()
  "Disable enabled themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun /config/util/harish-theme ()
  "Load Harish theme."
  (interactive)
  (load-theme 'harish t))


(provide '/config/util)
