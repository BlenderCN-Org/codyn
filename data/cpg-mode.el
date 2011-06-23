(defvar cpg-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.cpg\\'" . cpg-mode))

(defvar cpg-font-lock-keywords
  (list
   '("\\(\\(\\w\\|_\\)+\\)}?[ \t]*\\(\\?=\\|<=\\|=\\)" 1 font-lock-variable-name-face)
   '("\\(@\\(\\w\\|_\\)*\\(\\[.*\\]\\)?\\)" 1 font-lock-constant-face)
   '("\\<\\(define\\|or\\|network\\|templates\\|functions\\|integrator\\|interface\\)\\>" . font-lock-keyword-face)
   '("\\<\\(group\\|state\\|link\\|on\\|from\\|to\\|input-file\\)\\>" . font-lock-keyword-face)
   '("\\<\\(polynomial\\|piece\\)\\>" . font-lock-keyword-face)
   '("\\<\\(layout\\|left-of\\|right-of\\|above\\|below\\|at\\|of\\)\\>" . font-lock-keyword-face)
   '("\\<\\(delete\\|property\\|action\\)\\>" . font-lock-keyword-face)
   '("\\<\\(integrated\\|in\\|out\\|once\\)\\>" . font-lock-type-face)
   '("\\<\\(debug\\|context\\|selector\\)\\>" . font-lock-keyword-face)
   '("\\<\\(import\\|as\\|include\\)\\>" . font-lock-keyword-face)
   '("\\<\\(root\\|self\\|states\\|links\\|count\\)\\>" . font-lock-keyword-face)
   '("\\<\\(children\\|parent\\|first-child\\|last-child\\|first\\|last\\|subset\\|siblings\\)\\>" . font-lock-keyword-face)
   '("\\<\\(bidirectional\\|proxy\\|each\\)\\>" . font-lock-type-face)
   )
  "Highlighting for cpg mode")

(defun previous-non-empty-line ()
  "Moves to the first previous non-empty line"
  (interactive)
  (while (progn (forward-line -1)
                (looking-at "^[ \t]*$"))))

(defun cpg-indent-line ()
  "Indent current line as CPG code"
  (interactive)
  (let ((start-pos (point-marker)) (was-in-text t))
    (if (looking-back "^[ \t]*")
        (setq was-in-text nil))
    (beginning-of-line)
    (if (bobp)
        (indent-line-to 0)
      (let ((not-indented t) cur-indent)
        (if (looking-at "[^#\n]*}[ \t]*$")
            (progn
              (save-excursion
                (previous-non-empty-line)
                (if (looking-at "[^#\n]*{[ \t]*$")
                    (setq cur-indent (current-indentation))
                  (setq cur-indent (- (current-indentation) default-tab-width))))
              (if (< cur-indent 0)
                  (setq cur-indent 0)))
          (save-excursion
            (while not-indented
              (previous-non-empty-line)
              (if (looking-at "[^#\n]*}[ \t]*$")
                  (progn
                    (setq cur-indent (current-indentation))
                    (setq not-indented nil))
                (if (looking-at "[^#\n]*{[ \t]*$")
                    (progn
                      (setq cur-indent (+ (current-indentation) default-tab-width))
                      (setq not-indented nil))
                  (if (bobp)
                      (setq not-indented nil)))))))
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0))))
    (if was-in-text
        (goto-char start-pos))))
            
(defvar cpg-mode-syntax-table
  (let ((cpg-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# ".12" cpg-mode-syntax-table)
    (modify-syntax-entry ?# "<" cpg-mode-syntax-table)
    (modify-syntax-entry ?\n ">" cpg-mode-syntax-table)
    (modify-syntax-entry ?\n ">b" cpg-mode-syntax-table)
    cpg-mode-syntax-table)
  "Syntax table for the cpg-mode")

(defun cpg-mode ()
  "Major mode for editing CPG files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table cpg-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(cpg-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'cpg-indent-line)
  (setq major-mode 'cpg-mode)
  (setq mode-name "CPG")
  (setq comment-start "#")
  (run-hooks 'cpg-mode-hook))

(provide 'cpg-mode)
