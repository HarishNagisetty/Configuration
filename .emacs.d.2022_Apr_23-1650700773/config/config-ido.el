;; -*- lexical-binding: t -*-

(setq ido-enable-flex-matching t)

(/core/boot/require-package 'ido)
(require 'ido)

(ido-mode 1)

(provide '/config/ido)
