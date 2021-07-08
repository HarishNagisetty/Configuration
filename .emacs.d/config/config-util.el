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
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 0)
    (linum-mode 0)))

(defun /config/util/line-numbers ()
  "Enable line numbering for the current buffer."
  (interactive)
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 1)
    (linum-mode 1)))

(defun /config/util/toggle-line-numbers ()
  "Toggle line numbering for the currrent buffer."
  (interactive)
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 'toggle)
    (linum-mode 'toggle)))

(defun /config/util/center-buffer (buffer-width-chars)
  "Position buffer of width BUFFER-WIDTH at the center of the screen."
  (interactive (list (read-number "Buffer Width: " 80)))
  (if (> buffer-width-chars 0)
      ;; ** Fringe Mode
      ;;(progn
      ;;  (/config/util/no-line-numbers)
      ;;  (set-fringe-mode
      ;;   (/ (- (frame-pixel-width)
      ;;         (* buffer-width-chars (frame-char-width)))
      ;;      2)))
      ;; ** set-window-margins
      ;;(let ((margin-size (/ (- (frame-width) buffer-width-chars) 2)))
      ;;  (set-window-margins nil margin-size margin-size)
      ;;  (/config/util/no-line-numbers))
      (let ((margin-size (/ (- (frame-width) buffer-width-chars) 2)))
        (setq left-margin-width margin-size)
        (setq right-margin-width margin-size)
        (/config/util/no-line-numbers)
        (set-window-buffer (selected-window) (current-buffer)))
    (/config/util/line-numbers)
    (setq left-margin-width 0)
    (setq right-margin-width 0)
    (set-window-buffer (selected-window) (current-buffer))))

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

(defun /config/util/position-frame ()
  "Size and position frame in a sensible way."
  (interactive)
  (set-frame-width (selected-frame) 110)
  (set-frame-height (selected-frame) 33)
  (set-frame-position (selected-frame) 0 0))

(provide '/config/util)
