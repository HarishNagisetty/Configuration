;; -*- lexical-binding: t -*-

(setq whitespace-style '(tabs tab-mark))

(/core/boot/require-package 'whitespace)
(require 'whitespace)

(global-whitespace-mode 1)

(provide '/config/whitespace)
