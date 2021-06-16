;; -*- lexical-binding: t -*-

; Add all files in elisp/ to load-path.
(let ((base (concat user-emacs-directory "elisp/")))
  (add-to-list 'load-path base)
  (dolist (dir (directory-files base t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))



(defmacro /core/boot/measure-load (target expr)
  "Measures load time."
  (declare (indent defun))
  `(let ((elapsed)
         (start (current-time)))
     (prog1
         (apply (car ,expr) (cdr ,expr))
       (with-current-buffer (get-buffer-create "*Load Times*")
         (when (= 0 (buffer-size))
           (insert
            (format "| %-30s | %-23s | elapsed  |\n" "feature" "timestamp"))
           (insert "|--------------------------------+-------------------------+----------|\n"))
         (goto-char (point-max))
         (setq elapsed (float-time (time-subtract (current-time) start)))
         (insert (format "| %-30s | %s | %f |\n"
                         ,target
                         (format-time-string "%Y-%m-%d %H:%M:%S.%3N"
                                             (current-time))
                         elapsed))))))

(advice-add 'load :around
            (lambda (&rest args)
              (/core/boot/measure-load (nth 1 args) args)))

(advice-add 'require :around
            (lambda (&rest args)
              (if (memq (nth 1 args) features)
                  ;; If already loaded, don't count time.
                  (apply (car args) (cdr args))
                (/core/boot/measure-load (nth 1 args) args))))



(defun /core/boot/require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))



(defmacro /core/boot/after (feature &rest body)
  "Executes BODY after FEATURE has been loaded.
FEATURE may be any one of:
    'evil              => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]       => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))
"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))

(provide '/core/boot)
