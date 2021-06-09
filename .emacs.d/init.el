(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load custom.el without an error if file does not exist.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Make sure all packages are installed
(package-install-selected-packages)

;; Load personal configuration
(add-to-list 'load-path "~/.emacs.d/user-init/")
(require 'user-init)

(provide 'init)
