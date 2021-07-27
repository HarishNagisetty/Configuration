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

(require 'verilog3-mode)
(/core/boot/after 'verilog3-mode
  (add-to-list
   'auto-mode-alist '("\\.\\(sv\\|v\\|svh\\|vh\\|vinc\\)\\'"
                      . verilog3-mode)))

(provide '/config/verilog)
