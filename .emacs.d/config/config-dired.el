;; -*- lexical-binding: t -*-

(setq dired-listing-switches "-alhgG")

(add-hook 'dired-mode-hook '/config/util/no-line-numbers)

(provide '/config/dired)
