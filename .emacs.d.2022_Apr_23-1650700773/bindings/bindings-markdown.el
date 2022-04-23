;; -*- lexical-binding: t -*-

(defun /bindings/markdown/follow-link-on-line ()
  "Follow the first markdown link on the current line."
  (interactive)
  (save-match-data
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      (string-match "\\](\\(.*\\))" line)
      (let ((path (match-string 1 line)))
        (if (file-exists-p path)
            (find-file path)
          (message "Invalid File: %s" path))))))

(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map
              (kbd "<return>") #'/bindings/markdown/follow-link-on-line)))

(provide '/bindings/markdown)
