;;; user-init-key-chord.el

(setq key-chord-two-keys-delay 0.5)

(require 'key-chord)

(key-chord-mode t)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(provide 'user-init-key-chord)
