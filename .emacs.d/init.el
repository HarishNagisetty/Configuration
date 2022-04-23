;; -*- lexical-binding: t -*-

;; Measure startup time.
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time
                              (time-subtract
                               (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))


(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (user-init-directory (concat user-emacs-directory "init/"))
      (user-packages-directory (concat user-emacs-directory "init/packages/"))
      (user-bindings-directory (concat user-emacs-directory "init/bindings/"))
      (user-elisp-directory (concat user-emacs-directory "elisp/")))

  ;; Setup package.el.
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")))
  (require 'package)
  (package-initialize)

  ;; Measure library load times.
  (load-file (concat user-init-directory "measure-load.el"))

  ;; Load custom.el.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load-file custom-file))

  ;; Add elisp directory to load-path.
  (when (file-directory-p user-elisp-directory)
    (add-to-list 'load-path user-elisp-directory)
    (dolist (dir (directory-files user-elisp-directory t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir))))

  (load-file (concat user-init-directory "util.el"))
  (load-file (concat user-init-directory "basic-settings.el"))
  (load-file (concat user-init-directory "c-language.el"))
  (load-file (concat user-init-directory "require-package.el"))

  ;; Load and configure packages.
  (load-file (concat user-packages-directory "undo-fu.el"))
  (load-file (concat user-packages-directory "evil.el"))
  (load-file (concat user-packages-directory "ido.el"))
  (load-file (concat user-packages-directory "key-chord.el"))
  (load-file (concat user-packages-directory "company.el"))
  (load-file (concat user-packages-directory "ledger.el"))
  (load-file (concat user-packages-directory "markdown.el"))
  (load-file (concat user-packages-directory "htmlize.el"))
  (load-file (concat user-packages-directory "org.el"))
  (load-file (concat user-packages-directory "whitespace.el"))
  (load-file (concat user-packages-directory "which-key.el"))
  (load-file (concat user-packages-directory "verilog.el"))
  (load-file (concat user-packages-directory "verilog3.el"))

  ;; Set keybindings.
  (load-file (concat user-bindings-directory "core.el"))
  (load-file (concat user-bindings-directory "dired.el"))
  (load-file (concat user-bindings-directory "evil.el"))
  (load-file (concat user-bindings-directory "org.el"))
)
