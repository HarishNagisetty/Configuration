;; -*- lexical-binding: t -*-

(defvar verilog3-indent-offset 2
  "Indentation offset for Verilog3 mode.")

;;; Font Lock

(let* ((verilog3-type-font-keywords
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

       (verilog3-pragma-keywords
        (eval-when-compile
          (regexp-opt
           '("surefire" "0in" "auto" "leda" "rtl_synthesis" "synopsys"
             "verilint" ) nil)))

       (verilog3-ams-keywords
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

       (verilog3-font-general-keywords
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

       (verilog3-font-grouping-keywords
        (eval-when-compile
          (regexp-opt
           '( "begin" "end" ) nil))))

  ;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
  (setq verilog3-font-lock-keywords
        (list
         ;; Fontify all builtin keywords
         (concat "\\<\\(" verilog3-font-general-keywords "\\|"
                 ;; And user/system tasks and functions
                 "\\$[a-zA-Z][a-zA-Z0-9_\\$]*"
                 "\\)\\>")
         ;; Fontify all types
         (cons (concat "\\<\\(" verilog3-font-grouping-keywords "\\)\\>")
               'font-lock-type-face)
         (cons (concat "\\<\\(" verilog3-type-font-keywords "\\)\\>")
               'font-lock-type-face)
         ;; Fontify Verilog-AMS keywords
         (cons (concat "\\<\\(" verilog3-ams-keywords "\\)\\>")
               'font-lock-type-face)))

  (setq verilog3-font-lock-keywords-1
        (append verilog3-font-lock-keywords
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

  (setq verilog3-font-lock-keywords-2
        (append verilog3-font-lock-keywords-1
                (list
                 ;; Fontify pragmas
                 (concat "\\(//\\s-*\\(" verilog3-pragma-keywords "\\)\\s-.*\\)")
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

  (setq verilog3-font-lock-keywords-3
        verilog3-font-lock-keywords-2))

;;; Syntax Table

(defvar verilog3-mode-syntax-table
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

(defvar verilog3-indent-matching-keywords
  '(
    ;; B
    ("begin" . "end")
    ;; C
    ("case" . "endcase")
    ("casex" . "endcase")
    ("casez" . "endcase")
    ("class" . "endclass")
    ("clocking" . "endclocking")
    ("covergroup" . "endgroup")
    ("config" . "endconfig")
    ("connectmodule" . "endconnectmodule")
    ;; F
    ("fork" . "join")
    ("fork" . "join_any")
    ("fork" . "join_none")
    ("function" . "endfunction")
    ;; G
    ("generate" . "endgenerate")
    ;; I
    ("interface" . "endinterface")
    ;; M
    ("module" . "endmodule")
    ("macromodule" . "endmodule")
    ;; P
    ("program" . "endprogram")
    ("package" . "endpackage")
    ("primitive" . "endprimitive")
    ("property" . "endproperty")
    ;; R
    ("randcase" . "endcase")
    ;; S
    ("specify" . "endspecify")
    ("sequence" . "endsequence")
    ("randsequence" . "endsequence")
    ;; T
    ("task" . "endtask")
    ("table" . "endtable")
    )
  "All matching keywords for indentation.")

(defvar verilog3-indent-one-line-keywords
  '("if"
    "else"
    "initial"
    "final"
    "always"
    "always_latch"
    "always_ff"
    "always_comb")
  "Keywords that require indentation only for one statement.")

(defvar verilog3-left-aligned-keywords
  '("`begin_keywords"
    "`celldefine"
    "`default_nettype"
    "`define"
    "`else"
    "`elsif"
    "`end_keywords"
    "`endcelldefine"
    "`endif"
    "`ifdef"
    "`ifndef"
    "`include"
    "`line"
    "`nounconnected_drive"
    "`pragma"
    "`resetall"
    "`timescale"
    "`unconnected_drive"
    "`undef"
    "`undefineall")
  "Keywords (directives) that should be left aligned.")

(defun verilog3-equivalent-regexp (type kw)
  "Returns a regexp to match keywords equivalent to keyword KW of TYPE. TYPE
may be :begin or :end"
  (let (matchlist eqlist)
    ;; Populate "matchlist". This is a list of all keywords that match KW.
    (dolist (pair verilog3-indent-matching-keywords)
      (when (equal kw (if (eq type :end) (cdr pair) (car pair)))
        (let ((match (if (eq type :end) (car pair) (cdr pair))))
          (setq matchlist (append (list match) matchlist)))))
    ;; For each match, search for it's matching keyword.
    (dolist (match matchlist)
      (dolist (pair verilog3-indent-matching-keywords)
        (when (equal match (if (eq type :end) (car pair) (cdr pair)))
          (let ((match2 (if (eq type :end) (cdr pair) (car pair))))
            (setq eqlist (append (list match2) eqlist))))))
    (verilog3-keyword-regexp eqlist)))

(defun verilog3-matching-regexp (type kw)
  "Returns a regexp to match keyword KW of TYPE. TYPE may be :begin or :end."
  (let (matchlist)
    (dolist (pair verilog3-indent-matching-keywords)
      (when (equal kw (if (eq type :end) (cdr pair) (car pair)))
        (let ((match (if (eq type :end) (car pair) (cdr pair))))
          (setq matchlist (append (list match) matchlist)))))
    (verilog3-keyword-regexp matchlist)))

(defun verilog3-keyword-regexp (keywords)
  "For a list of keywords, return a regexp that will match any of them."
  (let (pat)
    (dolist (kw keywords)
      (if pat
            (setq pat (concat pat "\\|\\<" kw))
        (setq pat (concat "\\<" kw)))
      (when pat
        (setq pat (concat pat "\\>"))))
    pat))

(defun verilog3-backward-token ()
  "Copied from SMIE."
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-backward "."))
              (skip-syntax-backward "w_'"))
          (point))))

(defun verilog3-forward-token ()
  "Copied from SMIE."
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-forward "."))
              (skip-syntax-forward "w_'"))
          (point))))

(defun verilog3-comment-or-string-p ()
  "Is POINT in a comment or string?"
    (or (nth 3 (syntax-ppss))
        (nth 4 (syntax-ppss))))

(defun verilog3-backward-sexp (token)
  "Skip to the matching keyword of TOKEN. If TOKEN is empty or nil, try
backward-sexp."
  (if (zerop (length token))
      (condition-case nil
          (backward-sexp 1)
        (scan-error nil))
    (let* ((matching-kw (verilog3-matching-regexp :end token))
           (equivalent-kw (verilog3-equivalent-regexp :end token)))
      (when matching-kw
        (when (not equivalent-kw) (error "Did not find equivalent keywords"))
        (let ((sp (point))
              (pat (concat matching-kw "\\|\\(" equivalent-kw "\\)"))
              (open-count 1))
          ;; Keep going until we found our matching keyword or until there are
          ;; no more matches.
          (while (and (> open-count 0)
                      (re-search-backward pat nil t))
            ;; Only consider this match if it's not a string or comment or 
            ;; a special begin keyword.
            (when (not (or (verilog3-comment-or-string-p)
                           (verilog3-special-begin-keyword-p
                            (match-string 0))))
              (setq open-count
                    (if (match-end 1) (1+ open-count) (1- open-count)))))
          ;; If our search didn't succeed, go back to the original position.
          (when (> open-count 0) (goto-char sp)))))))

(defun verilog3-backward-stride (&optional stop-token)
  "Move backward to the last unbalanced token or STOP-TOKEN. Return nil if
something unexpected happens."
  (let ((begin-keywords (mapcar #'car verilog3-indent-matching-keywords))
        (end-keywords (mapcar #'cdr verilog3-indent-matching-keywords))
        token
        end-of-statement)
    (condition-case nil
        (catch 'return
          (while t
            ;; Fetch new token and skip any sexp. If point didn't move, Return nil.
            (let ((sp (point)))
              (setq token (verilog3-backward-token))
              (when (member token begin-keywords) (throw 'return token))
              (when (and stop-token (equal token stop-token))
                (throw 'return token))
              ;; If we see a semicolon or an `end' keyword, all one-statement
              ;; indentation is considered to have ended. The constraint block
              ;; allows `if' statements to terminate with "}". Let's use that as
              ;; an end token too.
              (when (or (equal (string (char-before)) "}")
                        (member token '(";" "end")))
                (setq end-of-statement t))
              (verilog3-backward-sexp token)
              (when (save-excursion (backward-char 1) (looking-at "\\s("))
                (throw 'return "("))
              ;; If we haven't moved...
              (when (or (>= (point) sp) (null token)) (throw 'return nil)))
            ;; If we meet a one-statement keyword without meeting a full statement
            ;; before, return.
            (when (and (member token verilog3-indent-one-line-keywords)
                       (not end-of-statement))
              (throw 'return token))))
      (error nil))))

(defun verilog3-forward-comment-same-line ()
  "Skip comments but stay on same line."
  (let ((sp (point))
        (mp (line-end-position)))
    (forward-comment (point-max))
    (when (> (point) mp) (goto-char sp))))

(defun verilog3-special-begin-keyword-p (tok)
  "Is this a special case where the begin keyword means something else? Point
should be just before TOK. Example: While `fork' generally starts a new block,
it means something else when preceeded by `wait'."
  (cond
   ;; We meet a "property" keyword, but it's preceeded by another keyword
   ;; that indicates it's not part of a matching pair.
   ((save-excursion
      (and (member tok '("property"))
           (member (verilog3-backward-token) '("assert"
                                               "assume"
                                               "cover"
                                               "restrict"))))
    t)
   ;; "wait fork"
   ;; "disable fork"
   ((save-excursion
      (and (member tok '("fork"))
           (member (verilog3-backward-token) '("wait"
                                               "disable"))))
    t)
   ;; "pure virtual function"
   ;; "pure virtual task"
   ((save-excursion
      (and (member tok '("function" "task"))
           (member (verilog3-backward-token) '("virtual"))
           (member (verilog3-backward-token) '("pure"))))
    t)
   ;; "extern", "import"
   ((and (member tok '("function" "task" "module"))
         ;; Find the previous mention without skipping any unbalanced
         ;; keywords.
         (or (save-excursion
               (equal (verilog3-backward-stride "extern") "extern"))
             (save-excursion
               (equal (verilog3-backward-stride "import") "import"))
             (save-excursion
               (equal (verilog3-backward-stride "export") "export"))))
    t)
   ))

(defun verilog3-indent-calculate (&optional savep)
  "Calculate indentation for the current line based on previous unmatched
keywords and the keyword after point."
  (when (verilog3-comment-or-string-p) nil)
  (let ((savep (or savep (point)))
        (tok (verilog3-backward-stride)))
    (cond
     ;; We meet an open paren and we're looking at a close paren.
     ((and (equal tok "(")
           (save-excursion
             (goto-char savep)
             (verilog3-forward-comment-same-line)
             (looking-at "\\s)")))
      (save-excursion
        (let ((save-col (current-column))
              (save-eol (line-end-position))
              (save-pt (point)))
          (verilog3-forward-token)
          ;; Check whether there is content after the open parenthesis on the
          ;; same line.
          (if (<= (point) save-eol)
              save-col
            (goto-char save-pt)
            (current-indentation)))))
     ;; We meet an open paren and there is content on the same line:
     ;; module test (token
     ;;              ^
     ((and (equal tok "(")
           (save-excursion
             (let ((save-col (current-column))
                   (save-eol (line-end-position)))
               (verilog3-forward-token)
               (if (> (point) save-eol)
                   nil
                 save-col)))))
     ;; We meet an open keyword and we're looking at a matching close keyword.
     ((let ((kw (verilog3-matching-regexp :begin tok)))
        (and kw
             (save-excursion
               (goto-char savep)
               (verilog3-forward-comment-same-line)
               (looking-at kw))))
      (current-indentation))
     ;; We meet a one statement keyword ("if", "always") and we're looking at a
     ;; "begin" keyword.
     ((and tok
           (string-match (verilog3-keyword-regexp
                          verilog3-indent-one-line-keywords)
                         tok)
           (save-excursion
             (goto-char savep)
             (verilog3-forward-comment-same-line)
             (looking-at (verilog3-keyword-regexp '("begin")))))
      (current-indentation))
     ;; "else"
     ((save-excursion
        (goto-char savep)
        (verilog3-forward-comment-same-line)
        (when (and (looking-at "\\<else\\>")
                   (equal (verilog3-backward-stride "if") "if"))
          (current-indentation))))
     ;; Left-aligned keywords
     ((save-excursion
        (goto-char savep)
        (verilog3-forward-comment-same-line)
        (when (looking-at
               (verilog3-keyword-regexp verilog3-left-aligned-keywords))
          0)))
     ;; Special-case begin keywords
     ((verilog3-special-begin-keyword-p tok)
      (save-excursion
        (verilog3-indent-calculate savep)))
     ;; If an unbalanced keyword was found, increase indent.
     (tok (+ (current-indentation) verilog3-indent-offset))
     ;; Default
     (t (current-indentation)))))

(defun verilog3-indent-line ()
  "Indent the current line. If point is before the current indent, move it to
the new indent."
  (interactive)
  (let* ((savep (point))
         (indent (or (with-demoted-errors
                       (save-excursion
                         (forward-line 0)
                         (skip-chars-forward " \t")
                         (if (>= (point) savep) (setq savep nil))
                         (or (verilog3-indent-calculate) 0)))
                     (current-indentation))))
    (if (not (numberp indent))
        ;; If something funny is used (e.g. `noindent'), return it.
        indent
      (if (< indent 0) (setq indent 0)) ; Just in case.
      (if savep
          (save-excursion (indent-line-to indent))
        (indent-line-to indent)))))

;;; Major Mode

(define-derived-mode verilog3-mode prog-mode "Verilog3"

  ;; Syntax

  (set-syntax-table verilog3-mode-syntax-table)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")

  ;; Font Lock

  (setq font-lock-defaults
        `((verilog3-font-lock-keywords
           verilog3-font-lock-keywords-1
           verilog3-font-lock-keywords-2
           verilog3-font-lock-keywords-3)
          nil nil nil))
  (setq font-lock-multiline t)

  ;; Indent
  (setq-local indent-line-function #'verilog3-indent-line)
  )

(provide 'verilog3-mode)
