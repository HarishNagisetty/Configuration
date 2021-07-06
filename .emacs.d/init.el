;; -*- lexical-binding: t -*-

(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time
                              (time-subtract
                               (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;; Speed up startup by setting gc-cons-threshold and
;; file-name-handler-alist.
(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (core-directory (concat user-emacs-directory "core/"))
      (bindings-directory (concat user-emacs-directory "bindings/"))
      (config-directory (concat user-emacs-directory "config/")))

  ;; If package-enable-at-startup is non-nil, package-initialize will
  ;; be run again after init.
  ;; https://www.reddit.com/r/emacs/comments/1rdstn/set_packageenableatstartup_to_nil_for_slightly/
  (setq package-enable-at-startup nil)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")))
  (require 'package)
  (package-initialize)

  ;; Load core-boot.el
  (load (concat core-directory "core-boot"))

  ;; Load custom.el
  (setq custom-file(concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Load all files in config and bindings directories.
  (cl-loop for file in (append (reverse (directory-files-recursively config-directory "\\.el$"))
                               (reverse (directory-files-recursively bindings-directory "\\.el$")))
           do (condition-case ex
                  (load (file-name-sans-extension file))
                ('error (with-current-buffer "*scratch*"
                          (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex)))))))

(provide 'init)
