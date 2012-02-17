(require 'json)

(defvar codyn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [f10] 'codyn-context-check)
    (define-key map "\C-c\C-a" 'codyn-align)
    map)
  "Keymap for the Codyn major mode")

(defun codyn-align (prefix)
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
        (align-regexp start end "\\(\s-*\\)\\(=\\||\\|<=\\)" 1 1 t)
      (align-regexp start end "\\(\s-*\\)|" 1 1 t))))

(defun codyn-context-check-error (context do-show)
  "Check the context JSON for error"
  (let ((data (cdr (assoc 'data context)))
        (status (string= "ok" (cdr (assoc 'status context))))
        start end)
    (if status
        (progn
          (codyn-context-clear nil nil)
          (if do-show
              (codyn-context-show context)))
      (let ((file-with-error (abbreviate-file-name (file-truename (cdr (assoc 'filename data))))))
        (save-current-buffer 
          (if (not (equal buffer-file-truename file-with-error))
              (find-file-other-window file-with-error))
          (save-excursion
            (goto-char (point-min))
            (forward-line (- (cdr (assoc 'line_start data)) 1))
            (forward-char (- (cdr (assoc 'column_start data)) 1))
            (setq start (point))
            (goto-char (point-min))
            (forward-line (- (cdr (assoc 'line_end data)) 1))
            (forward-char (cdr (assoc 'column_end data)))
            (setq end (point))
            (move-overlay codyn-context-overlay start end)
            (overlay-put codyn-context-overlay 'help-echo (cdr (assoc 'message data)))))
        (message (cdr (assoc 'message data)))))))

(defun codyn-context-parse (proc event buf do-show)
  "Parses the JSON from cdn-context"
  (with-current-buffer buf
    (if (or (string= event "finished\n")
            (string= event "exited abnormally with code 1\n"))
        (progn
          (let ((context (json-read-from-string (with-current-buffer (process-buffer proc)
                                                  (buffer-string)))))
            (codyn-context-check-error context do-show))
          (kill-buffer (process-buffer proc)))
      (progn
        (message (concat "cdn-context failed: " (substring event 0 -1)))))))

(defun codyn-context-print-defines (selections)
  (insert (propertize "\nDefines\n\n" 'face '(:inherit font-lock-type-face :weight bold)))
  (let ((defines (cdr (assoc 'defines (elt (cdr (assoc 'in (elt selections 0))) 0)))))
    (mapc (lambda (define)
            (insert (concat "\t"
                            (propertize (cdr (assoc 'key define)) 'face font-lock-variable-name-face)
                            (make-string (max 1 (- 15 (length (cdr (assoc 'key define))))) ? )
                            " = "
                            (propertize (cdr (assoc 'value (elt (cdr (assoc 'value define)) 0))) 'face font-lock-string-face)
                            "\n")))
              defines)))

(defun codyn-context-print-node (selection node)
  (let (elems (i-elem 0) i-level epxansions prefix i-expansion (output ""))
    (setq elems (cdr (assoc node selection)))
    (setq i-elem 0)
    (mapc (lambda (elem)
            (setq i-elem (+ i-elem 1))
            (setq expansions (cdr (assoc 'expansions elem)))
            (setq output (concat output
                                 "\n"
                                 (propertize (concat (capitalize (symbol-name node))
                                                     " "
                                                     (number-to-string i-elem)
                                                     ": ")
                                             'face font-lock-type-face)
                                 (propertize (concat (cdr (assoc 'typename elem))
                                                     " ")
                                             'face font-lock-keyword-face)
                                 (cdr (assoc 'name elem))
                                 "\n"))
            (setq i-level 0)
            (mapc (lambda (level)
                    (setq i-level (+ i-level 1))
                    (setq prefix (propertize (concat (make-string i-level ?@) ":") 'face font-lock-variable-name-face))
                    (setq output (concat output "\t" prefix (make-string (max 1 (- 7 i-level)) ?\ )))
                    (setq i-expansion -1)
                    (mapc (lambda (expansion)
                            (setq i-expansion (+ i-expansion 1))
                            (if (> i-expansion 0) (setq output (concat output " ")))
                            (setq output (concat output
                                                 (propertize (cdr (assoc 'value expansion)) 'face font-lock-string-face)
                                                 (propertize (concat " ("
                                                                     (number-to-string (cdr (assoc 'index expansion)))
                                                                     ")")
                                                             'face 'font-lock-comment-face))))
                          level)
                    (setq output (concat output "\n")))
                  expansions))
          elems)
    (insert (propertize output 'invisible node))))

(defun codyn-context-button (name other1 other2 hide-list msg)
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] `(lambda ()
                                      (interactive)
                                      (with-current-buffer "Codyn context"
                                        (setq buffer-invisibility-spec '(,@hide-list))
                                        (overlay-put (symbol-value (intern (concat ,name "-button"))) 'face '(:weight bold))
                                        (overlay-put (symbol-value (intern (concat ,other1 "-button"))) 'face '(:weight normal))
                                        (overlay-put (symbol-value (intern (concat ,other2 "-button"))) 'face '(:weight normal)))))
    (insert (propertize (capitalize name) 'keymap map 'mouse-face 'highlight 'help-echo msg)))
  (set (make-local-variable (intern (concat name "-button"))) (make-overlay (- (point) (length name)) (point))))

(defun codyn-context-show (context)
  (let ((data (elt (cdr (assoc 'data context)) 0))
        (buf (current-buffer))
        selections (i-selection 0) title)
    (if (equal buffer-file-truename (abbreviate-file-name (file-truename (cdr (assoc 'filename data)))))
        (save-selected-window
          (switch-to-buffer-other-window "Codyn context")
          (local-set-key "q" (lambda () (interactive) (kill-buffer-and-window)))
          (erase-buffer)
          (setq buffer-invisibility-spec ())
          (setq title (concat "Codyn context for " (file-name-nondirectory (buffer-name buf))))
          (insert (propertize (concat title "\n" (make-string (length title) ?-) "\n") 'face font-lock-comment-face))
          (setq selections (cdr (assoc 'selections data)))
          (codyn-context-print-defines selections)
          (insert (propertize "\n\nShow selections:\t" 'face '(:slant italic)))
          (codyn-context-button "in" "out" "both" '(out) "Click here to hide Out elements")
          (insert " ")
          (codyn-context-button "out" "in" "both" '(in) "Click here to hide In elements")
          (insert " ")
          (codyn-context-button "both" "in" "out" '() "Click here to show all elements")
          (overlay-put both-button 'face '(:weight bold))
          (insert "\n")
          (mapc (lambda (selection)
                  (setq i-selection (+ i-selection 1))
                  (insert (propertize (concat "\nSelection " (number-to-string i-selection) "\n") 'face '(:inherit font-lock-type-face :weight bold)))
                  (codyn-context-print-node selection 'in)
                  (codyn-context-print-node selection 'out))
                selections)
          (goto-char (point-min))))))


(defun codyn-context-update (buf do-show)
  "Runs codyn-context, parse JSON, check error"
  (with-current-buffer buf
    (if (file-readable-p buffer-file-name)
        (let (json-buf proc)
          (setq json-buf (generate-new-buffer (concat (buffer-name) " codyn-context")))
          (setq proc (start-process "codyn-context" json-buf "cdn-context" (file-name-nondirectory (buffer-file-name))
                                    "-l" (number-to-string (line-number-at-pos))
                                    "-c" (number-to-string (+ (current-column) 1))))
          (set-process-sentinel proc `(lambda (p e) (codyn-context-parse p e ,buf ,do-show)))))))

(defun codyn-context-update-for-hook ()
  "Calls codyn-context-update with the current buffer as argument"
  (if (eq major-mode 'codyn-mode)
      (codyn-context-update (current-buffer) nil)))

(defun codyn-context-check ()
  "Check the Codyn file syntax"
  (interactive)
  (if (eq major-mode 'codyn-mode)
      (progn
        (if (buffer-modified-p)
            (save-buffer) ; will trigger codyn-context-update
          (codyn-context-update (current-buffer) (called-interactively-p))))))

(defun codyn-context-clear (start stop)
  "Clears the context error face"
  (if (and (stringp (this-command-keys)) (eq major-mode 'codyn-mode))
      (delete-overlay codyn-context-overlay)))

(defvar codyn-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.cdn\\'" . codyn-mode))


;; (concat "\\_<" (regexp-opt '("integrated" "in" "out" "once")) "\\_>")
;; (concat "\\_<" (regexp-opt '("include" "parse" "object" "edge" "disabled" "functions" "interface" "polynomial" "import" "from" "to" "piece" "defines" "integrator" "node" "layout" "at" "above" "below" "left-of" "right-of" "of" "on" "all" "action" "delete" "debug-print" "apply" "unapply" "with" "when" "phase" "terminate" "any" "set" "templates" "templates-root" "root" "debug" "children" "parent" "first" "last" "subset" "edges" "siblings" "descendants" "ancestors" "unique" "count" "name" "self" "nodes" "imports" "objects" "variables" "actions" "if" "not" "from-set" "type" "has-flag" "has-template" "has-tag" "reverse" "input" "output" "inputs" "outputs" "input-name" "output-name" "recurse" "context" "as" "define")) "\\_>")
;; (concat "\\_<" (regexp-opt '("proxy" "bidirectional" "each" "settings" "no-self" "probability" "tag")) "\\_>")

(defvar codyn-font-lock-keywords
  (list
   '("\\(\\(\\w\\|_\\)+\\)[ \t]*\\(([^(]*)[ \t]*\\)?\\(\\?=\\|<=\\|=\\)" 1 font-lock-variable-name-face)
   '("@\\([0-9]+\\|[a-zA-Z_]\\(\\w\\|_\\)*\\)?\\(\\[[^[]*\\(\\[[^[]*\\]\\)?[^[]*\\]\\)*" . font-lock-constant-face)
   '("\\_<\\(?:in\\(?:tegrated\\)?\\|o\\(?:nce\\|ut\\)\\)\\_>"  . font-lock-type-face)
   '("\\_<\\(?:a\\(?:bove\\|ctions?\\|ll\\|n\\(?:cestors\\|y\\)\\|pply\\|[st]\\)\\|below\\|c\\(?:hildren\\|o\\(?:\\(?:ntex\\|un\\)t\\)\\)\\|d\\(?:e\\(?:bug\\(?:-print\\)?\\|fines?\\|lete\\|scendants\\)\\|isabled\\)\\|edges?\\|f\\(?:irst\\|rom\\(?:-set\\)?\\|unctions\\)\\|has-\\(?:flag\\|t\\(?:ag\\|emplate\\)\\)\\|i\\(?:f\\|mports?\\|n\\(?:clude\\|put\\(?:-name\\|s\\)?\\|te\\(?:grator\\|rface\\)\\)\\)\\|l\\(?:a\\(?:\\(?:s\\|you\\)t\\)\\|eft-of\\)\\|n\\(?:ame\\|o\\(?:des?\\|t\\)\\)\\|o\\(?:bjects?\\|utput\\(?:-name\\|s\\)?\\|[fn]\\)\\|p\\(?:ar\\(?:ent\\|se\\)\\|hase\\|iece\\|olynomial\\)\\|r\\(?:e\\(?:\\(?:cu\\|ve\\)rse\\)\\|ight-of\\|oot\\)\\|s\\(?:e\\(?:lf\\|t\\)\\|iblings\\|ubset\\)\\|t\\(?:e\\(?:mplates\\(?:-root\\)?\\|rminate\\)\\|o\\|ype\\)\\|un\\(?:apply\\|ique\\)\\|variables\\|w\\(?:hen\\|ith\\)\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(?:bidirectional\\|each\\|no-self\\|pro\\(?:\\(?:babilit\\|x\\)y\\)\\|settings\\|tag\\)\\_>" . font-lock-type-face))
  "Highlighting for codyn mode")

(defun previous-non-empty-line ()
  "Moves to the first previous non-empty line"
  (interactive)
  (while (progn (forward-line -1)
                (looking-at "^[ \t]*$"))))

(defun codyn-indent-line ()
  "Indent current line as Codyn code"
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

(defvar codyn-mode-syntax-table
  (let ((codyn-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# ".12" codyn-mode-syntax-table)
    (modify-syntax-entry ?# "<" codyn-mode-syntax-table)
    (modify-syntax-entry ?\n ">" codyn-mode-syntax-table)
    (modify-syntax-entry ?\n ">b" codyn-mode-syntax-table)
    (modify-syntax-entry ?- "_" codyn-mode-syntax-table)
    (modify-syntax-entry ?+ "." codyn-mode-syntax-table)
    (modify-syntax-entry ?/ "." codyn-mode-syntax-table)
    (modify-syntax-entry ?* "." codyn-mode-syntax-table)
    (modify-syntax-entry ?= "." codyn-mode-syntax-table)
    (modify-syntax-entry ?< "." codyn-mode-syntax-table)
    (modify-syntax-entry ?> "." codyn-mode-syntax-table)
    (modify-syntax-entry ?| "." codyn-mode-syntax-table)
    codyn-mode-syntax-table)
  "Syntax table for the codyn-mode")

(defun codyn-mode ()
  "Major mode for editing Codyn files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table codyn-mode-syntax-table)
  (use-local-map codyn-mode-map)
  (set (make-local-variable 'codyn-context-overlay) (make-overlay 0 0))
  (overlay-put codyn-context-overlay 'face font-lock-warning-face)
  (set (make-local-variable 'font-lock-defaults) '(codyn-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'codyn-indent-line)
  (setq major-mode 'codyn-mode)
  (setq mode-name "Codyn")
  (setq comment-start "#")
  (make-local-variable 'before-change-functions)
  (add-to-list 'before-change-functions 'codyn-context-clear)
  (add-hook 'after-save-hook 'codyn-context-update-for-hook)
  (run-hooks 'codyn-mode-hook)
  (codyn-context-update (current-buffer) nil))

(provide 'codyn-mode)
