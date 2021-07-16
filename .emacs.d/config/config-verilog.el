;; -*- lexical-binding: t -*-

(setq verilog-indent-level 2)
(setq verilog-indent-level-directive 2)
(setq verilog-indent-level-behavioral 2)
(setq verilog-indent-level-declaration 2)
(setq verilog-indent-level-module 2)

(setq verilog-indent-module-normal t)
(setq verilog-indent-lists nil)
(setq verilog-auto-endcomments nil)

(/core/boot/after 'verilog-mode
  (progn
    (define-key verilog-mode-map (kbd ";") 'self-insert-command)
    (define-key verilog-mode-map (kbd ":") 'self-insert-command)
    (define-key verilog-mode-map (kbd "RET") 'newline)
     (define-key verilog-mode-map (kbd "TAB") 'indent-for-tab-command)))

(provide '/config/verilog)
