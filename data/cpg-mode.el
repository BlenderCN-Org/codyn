(require 'json)

(defvar cpg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [f10] 'cpg-context-check)
    (define-key map "\C-c\C-a" 'cpg-align)
    map)
  "Keymap for the CPG major mode")

(defun cpg-align (prefix)
  "Align the region or containing block for '|' (and '=' if prefix)"
  (interactive "P")
  (let (start end)
    (if (and mark-active (/= (point) (mark)))
        (progn
          (setq start (region-beginning))
          (setq end (region-end)))
      (save-excursion
        (backward-up-list)
        (setq start (point))
        (forward-list)
        (setq end (point))))
    (if prefix
        (align-regexp start end "\\(\s-*\\)[=|]" 1 1 t)
      (align-regexp start end "\\(\s-*\\)|" 1 1 t))))

(defun cpg-context-check-error (cpg-file)
  "Check the parsed cpg-context JSON for error"
  (let ((data (cdr (assoc 'data cpg-context)))
        (status (string= "ok" (cdr (assoc 'status cpg-context))))
        start end)
    (if status
        (progn
          (with-current-buffer (get-file-buffer cpg-file)
            (cpg-context-clear nil nil))
          (cpg-context-show cpg-file))
      (with-current-buffer (get-file-buffer cpg-file)
        (if (equal (file-truename cpg-file) (file-truename (cdr (assoc 'filename data))))
            (save-excursion
              (goto-char (point-min))
              (forward-line (- (cdr (assoc 'lineno data)) 1))
              (forward-char (- (cdr (assoc 'column_start data)) 1))
              (setq start (point))
              (setq end (+ (point) (- (cdr (assoc 'column_end data)) (cdr (assoc 'column_start data))) 1))
              (move-overlay cpg-context-overlay start end)))
        (message (cdr (assoc 'message data)))))))

(defun cpg-context-parse (proc event)
  "Parses the JSON from cpg-context"
  (setq cpg-context (json-read-from-string
                     (with-current-buffer (process-buffer proc) (buffer-string))))
  (kill-buffer (process-buffer proc))
  (cpg-context-check-error (nth 1 (process-command proc)))
  (with-current-buffer (get-file-buffer (nth 1 (process-command proc)))
    ;; (setq cpg-context-updating nil)
    ))

(defun cpg-context-print-defines (selections)
  (insert (propertize "\nDefines\n\n" 'face font-lock-keyword-face))
  (let ((defines (cdr (assoc 'defines (elt (cdr (assoc 'out (elt selections 0))) 0)))))
    (mapc (lambda (define)
            (insert (concat "\t"
                            (propertize (cdr (assoc 'key define)) 'face font-lock-variable-name-face)
                            (make-string (max 1 (- 15 (length (cdr (assoc 'key define))))) ? )
                            " = "
                            (propertize (cdr (assoc 'value define)) 'face font-lock-string-face)
                            "\n")))
              defines)))

(defun cpg-context-print-group (selection group)
  (let (elems (i-elem 0) i-level epxansions prefix i-expansion)
    (setq elems (cdr (assoc group selection)))
    (setq i-elem 0)
    (mapc (lambda (elem)
            (setq i-elem (+ i-elem 1))
            (setq expansions (cdr (assoc 'expansions elem)))
            (if (> (length expansions) 0)
                (progn
                  (insert (propertize (concat "\n"
                                              (capitalize (symbol-name group))
                                              " "
                                              (number-to-string i-elem)
                                              "\n")
                                      'face font-lock-type-face))
                  (setq i-level 0)
                  (mapc (lambda (level)
                          (setq i-level (+ i-level 1))
                          (setq prefix (propertize (concat (make-string i-level ?@) ":") 'face font-lock-variable-name-face))
                          (insert (concat "\t" prefix (make-string (max 1 (- 7 i-level)) ?\ )))
                          (setq i-expansion -1)
                          (mapc (lambda (expansion)
                                  (setq i-expansion (+ i-expansion 1))
                                  (if (> i-expansion 0) (insert ", "))
                                  (insert (concat
                                           (propertize (cdr (assoc 'value expansion)) 'face font-lock-string-face)
                                           (propertize (concat " ("
                                                               (number-to-string (cdr (assoc 'index expansion)))
                                                               ")")
                                                       'face 'font-lock-comment-face))))
                                level)
                          (insert "\n"))
                        expansions))))
          elems)))

(defun cpg-context-show (cpg-file)
  ;; (if (and (eq major-mode 'cpg-mode) cpg-context-do-show)
  (if cpg-context-do-show
      (let ((buffer (get-file-buffer cpg-file))
            (data (elt (cdr (assoc 'data cpg-context)) 0))
            selections (i-selection 0) title)
        (if (equal (file-truename cpg-file) (file-truename (cdr (assoc 'filename data))))
            (save-selected-window
              (switch-to-buffer-other-window "CPG context")
              (erase-buffer)
              (setq title (concat "CPG context for " (file-name-nondirectory cpg-file)))
              (insert (propertize (concat title "\n" (make-string (length title) ?-) "\n") 'face font-lock-comment-face))
              (setq selections (cdr (assoc 'selections data)))
              (cpg-context-print-defines selections)
              (mapc (lambda (selection)
                      (setq i-selection (+ i-selection 1))
                      (insert (propertize (concat "\nSelection " (number-to-string i-selection) "\n") 'face font-lock-keyword-face))
                      (cpg-context-print-group selection 'in)
                      (cpg-context-print-group selection 'out))
                    selections)))))
  (setq cpg-context-do-show nil))


(defun cpg-context-update ()
  "Runs cpg-context, parse JSON, check error"
  ;; (if (and (eq major-mode 'cpg-mode) (not cpg-context-updating))
  (if (eq major-mode 'cpg-mode)
      (let (json-buf proc)
        ;; (setq cpg-context-updating t)
        (setq json-buf (generate-new-buffer (concat (buffer-name) " cpg-context")))
        (setq proc (start-process "cpg-context" json-buf "cpg-context" (file-name-nondirectory (buffer-file-name))
                                  "-l" (number-to-string (line-number-at-pos))
                                  "-c" (number-to-string (+ (current-column) 1))))
        (set-process-sentinel proc 'cpg-context-parse))))

(defun cpg-context-check ()
  "Check the CPG file syntax"
  (interactive)
  (if (eq major-mode 'cpg-mode)
      (progn
        (setq cpg-context-do-show (called-interactively-p))
        (if (buffer-modified-p)
            (save-buffer) ; will trigger cpg-context-update
          (cpg-context-update)))))

(defun cpg-context-clear (start stop)
  "Clears the cpg-context error face"
  (if (and (stringp (this-command-keys)) (eq major-mode 'cpg-mode))
      (delete-overlay cpg-context-overlay)))

(defvar cpg-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.cpg\\'" . cpg-mode))

(defvar cpg-font-lock-keywords
  (list
   '("\\(\\(\\w\\|_\\)+\\)}?[ \t]*\\(\\?=\\|<=\\|=\\)" 1 font-lock-variable-name-face)
   '("\\(\\(\\w\\|_\\)+\\)}?[ \t]*\\(\\?=\\|<=\\|=\\)" 1 font-lock-variable-name-face)
   '("\\(@\\(\\w\\|_\\)*\\(\\[.*\\]\\)?\\)" 1 font-lock-constant-face)
   '("\\_<\\(defines\\|or\\|network\\|templates\\|functions\\|integrator\\|interface\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(group\\|state\\|link\\|on\\|from\\|to\\|input-file\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(polynomial\\|piece\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(layout\\|left-of\\|right-of\\|above\\|below\\|at\\|of\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(delete\\|property\\|action\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(integrated\\|in\\|out\\|once\\)\\_>" . font-lock-type-face)
   '("\\_<\\(debug\\|context\\|selector\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(import\\|as\\|include\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(root\\|self\\|states\\|links\\|count\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(children\\|parent\\|first-child\\|last-child\\|first\\|last\\|subset\\|siblings\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(bidirectional\\|proxy\\|each\\)\\_>" . font-lock-type-face)
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
      (let (cur-indent closing)
        (save-excursion
          (setq closing (looking-at "[ \t]*}[ \t]*$"))
          (previous-non-empty-line)
          (setq cur-indent (current-indentation))
          (if closing
              (if (not (looking-at "[^#\n]*{[ \t]*$"))
                  (setq cur-indent (max 0 (- (current-indentation) default-tab-width))))
            (if (looking-at "[^#\n]*{[ \t]*$")
                (setq cur-indent (+ (current-indentation) default-tab-width)))))
        (indent-line-to cur-indent)))
    (if was-in-text
        (goto-char start-pos))))

(defvar cpg-mode-syntax-table
  (let ((cpg-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# ".12" cpg-mode-syntax-table)
    (modify-syntax-entry ?# "<" cpg-mode-syntax-table)
    (modify-syntax-entry ?\n ">" cpg-mode-syntax-table)
    (modify-syntax-entry ?\n ">b" cpg-mode-syntax-table)
    (modify-syntax-entry ?- "." cpg-mode-syntax-table)
    (modify-syntax-entry ?+ "." cpg-mode-syntax-table)
    (modify-syntax-entry ?/ "." cpg-mode-syntax-table)
    (modify-syntax-entry ?* "." cpg-mode-syntax-table)
    (modify-syntax-entry ?= "." cpg-mode-syntax-table)
    (modify-syntax-entry ?< "." cpg-mode-syntax-table)
    (modify-syntax-entry ?> "." cpg-mode-syntax-table)
    (modify-syntax-entry ?| "." cpg-mode-syntax-table)
    cpg-mode-syntax-table)
  "Syntax table for the cpg-mode")

(defun cpg-mode ()
  "Major mode for editing CPG files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table cpg-mode-syntax-table)
  (use-local-map cpg-mode-map)
  (set (make-local-variable 'cpg-context-overlay) (make-overlay 0 0))
  (overlay-put cpg-context-overlay 'face font-lock-warning-face)
  (set (make-local-variable 'font-lock-defaults) '(cpg-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'cpg-indent-line)
  (setq major-mode 'cpg-mode)
  (setq mode-name "CPG")
  (setq comment-start "#")
  (set (make-local-variable 'cpg-context) nil)
  (set (make-local-variable 'cpg-context-do-show) nil)
  ;; (set (make-local-variable 'cpg-context-updating) nil)
  (make-local-variable 'before-change-functions)
  (add-to-list 'before-change-functions 'cpg-context-clear)
  (add-hook 'after-save-hook 'cpg-context-check)
  (run-hooks 'cpg-mode-hook)
  (cpg-context-update))

(provide 'cpg-mode)
