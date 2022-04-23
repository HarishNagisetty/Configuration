;; -*- lexical-binding: t -*-

(/core/boot/after [key-chord evil /bindings/evil]
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(provide '/bindings/key-chord)
