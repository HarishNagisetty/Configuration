;; -*- lexical-binding: t -*-

(/core/boot/after [evil /bindings/core]
  (/bindings/core/define-prefix-keys evil-motion-state-map nil
    (/bindings/core/leader /bindings/core/global-leader-map)
    ))

(provide '/bindings/evil)
