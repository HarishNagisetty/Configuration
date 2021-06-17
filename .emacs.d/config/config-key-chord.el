;; -*- lexical-binding: t -*-

(setq key-chord-two-keys-delay 0.5)

(/core/boot/require-package 'key-chord)
(require 'key-chord)

(key-chord-mode 1)

(provide 'config-key-chord)
