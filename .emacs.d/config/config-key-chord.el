;; -*- lexical-binding: t -*-

(setq key-chord-two-keys-delay 0.5)

(/core/boot/require-package 'key-chord)
(require 'key-chord)

(/core/boot/after [key-chord evil]
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(provide 'config-key-chord)
