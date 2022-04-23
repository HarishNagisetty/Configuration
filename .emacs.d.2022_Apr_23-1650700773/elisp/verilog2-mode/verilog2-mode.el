;; -*- lexical-binding: t -*-

(require 'smie)

(defvar verilog2-indent-offset 2
  "Indentation offset for Verilog2 mode.")

;;; Font Lock

(let* ((verilog2-type-font-keywords
        (eval-when-compile
          (regexp-opt
           '("and" "buf" "bufif0" "bufif1" "cmos" "defparam" "event"
             "genvar" "highz0" "highz1" "inout" "input" "integer"
             "localparam" "mailbox" "nand" "nmos" "nor" "not" "notif0"
             "notif1" "or" "output" "parameter" "pmos" "pull0" "pull1"
             "pulldown" "pullup" "rcmos" "real" "realtime" "reg" "rnmos"
             "rpmos" "rtran" "rtranif0" "rtranif1" "semaphore" "signed"
             "specparam" "strong0" "strong1" "supply" "supply0" "supply1"
             "time" "tran" "tranif0" "tranif1" "tri" "tri0" "tri1" "triand"
             "trior" "trireg" "unsigned" "uwire" "vectored" "wand" "weak0"
             "weak1" "wire" "wor" "xnor" "xor"
             ;; 1800-2005
             "bit" "byte" "chandle" "const" "enum" "int" "logic" "longint"
             "packed" "ref" "shortint" "shortreal" "static" "string"
             "struct" "type" "typedef" "union" "var"
             ;; 1800-2009
             ;; 1800-2012
             "interconnect" "nettype" ) nil)))

       (verilog2-pragma-keywords
        (eval-when-compile
          (regexp-opt
           '("surefire" "0in" "auto" "leda" "rtl_synthesis" "synopsys"
             "verilint" ) nil)))

       (verilog2-ams-keywords
        (eval-when-compile
          (regexp-opt
           '("above" "abs" "absdelay" "abstol" "ac_stim" "access" "acos"
             "acosh" "aliasparam" "analog" "analysis" "asin" "asinh" "atan"
             "atan2" "atanh" "branch" "ceil" "connect" "connectmodule"
             "connectrules" "continuous" "cos" "cosh" "ddt" "ddt_nature"
             "ddx" "discipline" "discrete" "domain" "driver_update"
             "endconnectmodule" "endconnectrules" "enddiscipline" "endnature" "endparamset"
             "exclude" "exp" "final_step" "flicker_noise" "floor" "flow"
             "from" "ground" "hypot" "idt" "idt_nature" "idtmod" "inf"
             "initial_step" "laplace_nd" "laplace_np" "laplace_zd"
             "laplace_zp" "last_crossing" "limexp" "ln" "log" "max"
             "merged" "min" "nature" "net_resolution" "noise_table"
             "paramset" "potential" "pow" "resolveto" "sin" "sinh" "slew"
             "split" "sqrt" "tan" "tanh" "timer" "transition" "units"
             "white_noise" "wreal" "zi_nd" "zi_np" "zi_zd" "zi_zp"
             ;; Excluded AMS keywords: "assert" "cross" "string"
             ) nil)))

       (verilog2-font-general-keywords
        (eval-when-compile
          (regexp-opt
           '("always" "assign" "automatic" "case" "casex" "casez" "cell"
             "config" "deassign" "default" "design" "disable" "edge" "else"
             "endcase" "endconfig" "endfunction" "endgenerate" "endmodule"
             "endprimitive" "endspecify" "endtable" "endtask" "for" "force"
             "forever" "fork" "function" "generate" "if" "ifnone" "incdir"
             "include" "initial" "instance" "join" "large" "liblist"
             "library" "macromodule" "medium" "module" "negedge"
             "noshowcancelled" "posedge" "primitive" "pulsestyle_ondetect"
             "pulsestyle_onevent" "release" "repeat" "scalared"
             "showcancelled" "small" "specify" "strength" "table" "task"
             "use" "wait" "while"
             ;; 1800-2005
             "alias" "always_comb" "always_ff" "always_latch" "assert"
             "assume" "before" "bind" "bins" "binsof" "break" "class"
             "clocking" "constraint" "context" "continue" "cover"
             "covergroup" "coverpoint" "cross" "dist" "do" "endclass"
             "endclocking" "endgroup" "endinterface" "endpackage"
             "endprogram" "endproperty" "endsequence" "expect" "export"
             "extends" "extern" "final" "first_match" "foreach" "forkjoin"
             "iff" "ignore_bins" "illegal_bins" "import" "inside"
             "interface" "intersect" "join_any" "join_none" "local"
             "matches" "modport" "new" "null" "package" "priority"
             "program" "property" "protected" "pure" "rand" "randc"
             "randcase" "randsequence" "return" "sequence" "solve" "super"
             "tagged" "this" "throughout" "timeprecision" "timeunit"
             "unique" "virtual" "void" "wait_order" "wildcard" "with"
             "within"
             ;; 1800-2009
             "accept_on" "checker" "endchecker" "eventually" "global"
             "implies" "let" "nexttime" "reject_on" "restrict" "s_always"
             "s_eventually" "s_nexttime" "s_until" "s_until_with" "strong"
             "sync_accept_on" "sync_reject_on" "unique0" "until"
             "until_with" "untyped" "weak"
             ;; 1800-2012
             "implements" "soft" ) nil)))

       (verilog2-font-grouping-keywords
        (eval-when-compile
          (regexp-opt
           '( "begin" "end" ) nil))))

  ;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
  (setq verilog2-font-lock-keywords
        (list
         ;; Fontify all builtin keywords
         (concat "\\<\\(" verilog2-font-general-keywords "\\|"
                 ;; And user/system tasks and functions
                 "\\$[a-zA-Z][a-zA-Z0-9_\\$]*"
                 "\\)\\>")
         ;; Fontify all types
         (cons (concat "\\<\\(" verilog2-font-grouping-keywords "\\)\\>")
               'font-lock-type-face)
         (cons (concat "\\<\\(" verilog2-type-font-keywords "\\)\\>")
               'font-lock-type-face)
         ;; Fontify Verilog-AMS keywords
         (cons (concat "\\<\\(" verilog2-ams-keywords "\\)\\>")
               'font-lock-type-face)))

  (setq verilog2-font-lock-keywords-1
        (append verilog2-font-lock-keywords
                (list
                 ;; Fontify module definitions
                 (list
                  "\\<\\(\\(macro\\|connect\\)?module\\|primitive\\|class\\|program\\|interface\\|package\\|task\\)\\>\\s-*\\(\\sw+\\)"
                  '(1 font-lock-keyword-face)
                  '(3 font-lock-function-name-face prepend))
                 ;; Fontify function definitions
                 (list
                  (concat "\\<function\\>\\s-+\\(integer\\|real\\(time\\)?\\|time\\)\\s-+\\(\\sw+\\)" )
                  '(1 font-lock-keyword-face)
                  '(3 font-lock-constant-face prepend))
                 '("\\<function\\>\\s-+\\(\\[[^]]+\\]\\)\\s-+\\(\\sw+\\)"
                   (1 font-lock-keyword-face)
                   (2 font-lock-constant-face append))
                 '("\\<function\\>\\s-+\\(\\sw+\\)"
                   1 'font-lock-constant-face append))))

  (setq verilog2-font-lock-keywords-2
        (append verilog2-font-lock-keywords-1
                (list
                 ;; Fontify pragmas
                 (concat "\\(//\\s-*\\(" verilog2-pragma-keywords "\\)\\s-.*\\)")
                 ;; Fontify escaped names
                 '("\\(\\\\\\S-*\\s-\\)"  0 font-lock-function-name-face)
                 ;; Fontify macro definitions/ uses
                 '("`\\s-*[A-Za-z][A-Za-z0-9_]*" 0 (if (boundp 'font-lock-preprocessor-face)
                                                       'font-lock-preprocessor-face
                                                     'font-lock-type-face))
                 ;; Fontify delays/numbers
                 '("\\(@\\)\\|\\([ \t\n\f\r]#\\s-*\\(\\([0-9_.]+\\('s?[hdxbo][0-9a-fA-F_xz]*\\)?\\)\\|\\(([^()]+)\\|\\sw+\\)\\)\\)"
                   0 font-lock-type-face append)
                 ;; Fontify property/sequence cycle delays - these start with '##'
                 '("\\(##\\(\\sw+\\|\\[[^]]+\\]\\)\\)"
                   0 font-lock-type-face append)
                 ;; Fontify instantiation names
                 '("\\([A-Za-z][A-Za-z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face))))

  (setq verilog2-font-lock-keywords-3
        verilog2-font-lock-keywords-2))

;;; Syntax Table

(defvar verilog2-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Populate the syntax TABLE.
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?` "w" table) ; ` is part of definition symbols in Verilog
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\' "." table)

    ;; Set up TABLE to handle block and line style comments.
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    table)
  "Syntax table used in SysVerilog mode buffers.")

;;; Indentation

(defvar verilog2-smie-defun-keywords
  '(("module" . "endmodule")
    ("macromodule" . "endmodule")
    ("connectmodule" . "endconnectmodule")
    ("class" . "endclass")
    ("program" . "endprogram")
    ("interface" . "endinterface")
    ("package" . "endpackage")
    ("primitive" . "endprimitive")
    ("config" . "endconfig")
    ("function" . "endfunction")
    ("property" . "endproperty")
    ("task" . "endtask"))
  "These matching keywords have declarations that look like a statement and
  need special attention.")

(defvar verilog2-one-line-keywords
  '("else"
    "initial"
    "final"
    "always"
    "always_latch"
    "always_ff"
    "always_comb")
  "Keywords that require simple indentation only for one line.")

(defvar verilog2-smie-matching-keywords
  (append
   verilog2-smie-defun-keywords
   '(("begin" . "end")
     ("table" . "endtable")
     ("specify" . "endspecify")
     ("generate" . "endgenerate")
     ("clocking" . "endclocking")
     ("fork" . "join")
     ("fork" . "join_any")
     ("fork" . "join_none")
     ("case" . "endcase")
     ("casex" . "endcase")
     ("casez" . "endcase")
     ("randcase" . "endcase")))
  "All matching keywords for indentation.")

(defvar verilog2-smie-operator-table
  `((assoc ";")
    ,(push 'right verilog2-one-line-keywords)
    (assoc ",")
    (right "=" "<=")
    (assoc "&&" "||")
    (assoc "|" "&")
    (assoc "+" "-")
    (assoc "*" "/")
    (assoc "<<" "<<<" ">>" ">>>")
    (assoc "==" "===" "!=" "!==")
    (assoc "?")
    (assoc ":")
    (right "#"))
  "Operators for indentation. Increasing order of precedence.")

(defvar verilog2-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     `((exp (exp ";" exp)
            (block))
       (block ,@(mapcar (lambda (x)
                          (list (car x) 'exp (cdr x)))
                        verilog2-smie-matching-keywords)))
     '((assoc ";")))
    (smie-precs->prec2 verilog2-smie-operator-table)))
  "BNF grammar for the SMIE.")

(defun verilog2-smie-backward-token ()
  (let ((tkn (smie-default-backward-token)))
    (cond
     ((equal tkn "end")
      "end")
     (t tkn))))

(defun verilog2-smie-forward-token ()
  (let ((tkn (smie-default-forward-token)))
    (cond
     ((equal tkn "end")
      "end")
     (t tkn))))

(defun verilog2-smie-rules (kind token)
  "Tweaks to the default SMIE indentation rules."
  nil
  (cond
   ;; Set indent offset.
   ((and (equal kind :elem) (equal token 'basic)) verilog2-indent-offset)
   ;; No indentation for continued expressions. Set :list-intro for all tokens.
   ((equal kind :list-intro) t)
   ;; :before "begin"
   ((and (equal kind :before)
         (member token (mapcar #'car verilog2-smie-matching-keywords)))
    (smie-indent-keyword ";"))
   ;; :after "end"
   ((and (equal kind :after)
         (member token (mapcar #'cdr verilog2-smie-matching-keywords)))
    (smie-rule-parent))
   ;; Set indentation for parenthesis in a defun/declaration.
   ((and (equal kind :before)
         (member token '("(" "#"))
         (save-excursion
           (smie-backward-sexp ";")
           (if (member (funcall smie-backward-token-function)
                       (mapcar #'car verilog2-smie-defun-keywords))
               `(column . ,(smie-indent-virtual))))))
   ;; Set indentation after "if" and others:
   ;; if (condition)
   ;;   statement;
   ;; ^^ 
   ((and (equal kind :after)
         (equal token ")")
         (save-excursion
           (forward-char 1)
           (backward-sexp 1)
           (let ((prev (funcall smie-backward-token-function))
                 (base (current-column)))
             (when (member prev '("if" "case" "casex" "casez" "property" "@"))
              ;; For some preceeding tokens, consider that as the base if it's on
              ;; the same line.
              ;;
              ;; else if (condition)
              ;; ^ statement;
              ;; |
              ;; Start indententaion from here.
              (let ((lbeg (line-beginning-position)))
                (while (and (member (funcall smie-backward-token-function)
                                    '("else" "end" "always" "always_ff"))
                            (<= lbeg (point)))
                  (setq base (current-column))))
              `(column . ,(+ base verilog2-indent-offset)))))))
   ;; Set indentation for tokens after one-line keywords.
   ((and (equal kind :after)
         (member token verilog2-one-line-keywords)
         `(column . ,(+ (current-column) verilog2-indent-offset))))
   ;; Set indentation for the first "statement" of a defun:
   ;;
   ;; module test_module (
   ;;   parameter P1 = 5
   ;; ) (
   ;;   input i
   ;; );
   ;;  ^
   ;; "test_module (...) (...);" looks like a statement, but should be
   ;; indented differently.
   ;;
   ((and (equal kind :before)
         (equal token ";")
         (when (save-excursion
            (smie-backward-sexp ";")
            (member (funcall smie-backward-token-function)
                    (append '("case" "casex" "casez")
                            (mapcar #'car verilog2-smie-defun-keywords))))
      verilog2-indent-offset)))
   ))

;;; Major Mode

(define-derived-mode verilog2-mode prog-mode "Verilog2"

  ;; Syntax

  (set-syntax-table verilog2-mode-syntax-table)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")

  ;; Font Lock

  (setq font-lock-defaults
        `((verilog2-font-lock-keywords
           verilog2-font-lock-keywords-1
           verilog2-font-lock-keywords-2
           verilog2-font-lock-keywords-3)
          nil nil nil))
  (setq font-lock-multiline t)

  ;; SMIE

  (smie-setup verilog2-smie-grammar #'verilog2-smie-rules))

(provide 'verilog2-mode)
