;;; user-init-custom.el --- Add custom commands to emacs

(setq hn-evil-leader ", ")
(setq hn-evil-localleader "SPC ")

(defun hn-font-exists-p (font)
  "Check if font is available"
  (if (null (x-list-fonts font)) nil t))

(defun hn-set-comment-char (char)
  "Set comment character for current buffer."
  (interactive "sComment Character: ")
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

(defun hn-dired-up-directory ()
  "Go to parent directory and hide details if previously hidden."
  (interactive)
  (let ((hide-details (bound-and-true-p dired-hide-details-mode)))
    (dired-up-directory)
    (when hide-details
      (dired-hide-details-mode))))

(defun hn-dired-open-other-window-if-file ()
  "Open in same window if directory, other window if file"
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (hide-details (bound-and-true-p dired-hide-details-mode)))
    (if (file-directory-p file)
        (progn
          (dired-find-file)
          (define-key evil-normal-state-local-map
            (kbd "<return>") 'hn-dired-open-other-window-if-file)
          ;; If hide-details was previously set, do
          ;; the same for this buffer too.
          (when hide-details
            (dired-hide-details-mode)))
      (dired-find-file-other-window)
      (other-window -1))))

(defun hn-dired-open-sidebar ()
  "Open dired like a sidebar"
  (interactive)
  (let ((width (round (* 0.33 (window-width)))))
    (split-window-horizontally width)
    (dired default-directory)
    (dired-hide-details-mode)
    (define-key evil-normal-state-local-map
      (kbd "<return>") 'hn-dired-open-other-window-if-file)))

(defun hn-no-line-numbers ()
  "Disable line numbering for the current buffer."
  (interactive)
  (if (fboundp 'global-display-line-numbers-mode)
      (display-line-numbers-mode 0)
    (linum-mode 0)))

(defun hn-follow-markdown-link-on-line ()
  "Follow the first markdown link on the current line."
  (interactive)
  (save-match-data
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      (string-match "\\](\\([[:word:]/]*\\))" line)
      (let ((path (match-string 1 line)))
        (message "Path: %s" path)))))

(defun hn-open-scratch-buffer nil
  "Open/Create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(provide 'user-init-custom)