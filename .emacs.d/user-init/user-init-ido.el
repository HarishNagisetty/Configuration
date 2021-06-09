;;; user-init-ido.el

(setq ido-enable-flex-matching t)

(require 'ido)

(ido-mode 1)
(global-set-key (kbd "C-x r b")
                (lambda ()
                  (interactive)
                  (bookmark-jump
                   (ido-completing-read "Jump to bookmark: "
                                        (bookmark-all-names)))))

(provide 'user-init-ido)
