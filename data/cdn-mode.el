(require 'json)

(defvar cdn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [f10] 'cdn-context-check)
    (define-key map "\C-c\C-a" 'cdn-align)
    map)
  "Keymap for the CDN major mode")

(defun cdn-align (prefix)
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

(defun cdn-context-check-error (cdn-file)
  "Check the parsed cdn-context JSON for error"
  (let ((data (cdr (assoc 'data cdn-context)))
        (status (string= "ok" (cdr (assoc 'status cdn-context))))
        start end)
    (if status
        (progn
          (with-current-buffer (get-file-buffer cdn-file)
            (cdn-context-clear nil nil))
          (cdn-context-show cdn-file))
      (with-current-buffer (get-file-buffer cdn-file)
        (if (equal (file-truename cdn-file) (file-truename (cdr (assoc 'filename data))))
            (save-excursion
              (goto-char (point-min))
              (forward-line (- (cdr (assoc 'line_start data)) 1))
              (forward-char (- (cdr (assoc 'column_start data)) 1))
              (setq start (point))
              (goto-char (point-min))
              (forward-line (- (cdr (assoc 'line_end data)) 1))
              (forward-char (cdr (assoc 'column_end data)))
              (setq end (point))
              (move-overlay cdn-context-overlay start end)
              (overlay-put cdn-context-overlay 'help-echo (cdr (assoc 'message data)))))
        (message (cdr (assoc 'message data)))))))

(defun cdn-context-parse (proc event)
  "Parses the JSON from cdn-context"
  (if (or (string= event "finished\n")
          (string= event "exited abnormally with code 1\n"))
      (progn
        (setq cdn-context (json-read-from-string
                           (with-current-buffer (process-buffer proc) (buffer-string))))
        (kill-buffer (process-buffer proc))
        (cdn-context-check-error (nth 1 (process-command proc)))
        ;; (with-current-buffer (get-file-buffer (nth 1 (process-command proc)))
        ;;   (setq cdn-context-updating nil))
        )
    (with-current-buffer (get-file-buffer (nth 1 (process-command proc)))
      (setq cdn-context-do-show nil)
      (message (concat "cdn-context failed: " (substring event 0 -1))))))

(defun cdn-context-print-defines (selections)
  (insert (propertize "\nDefines\n\n" 'face '(:inherit font-lock-type-face :weight bold)))
  (let ((defines (cdr (assoc 'defines (elt (cdr (assoc 'in (elt selections 0))) 0)))))
    (mapc (lambda (define)
            (insert (concat "\t"
                            (propertize (cdr (assoc 'key define)) 'face font-lock-variable-name-face)
                            (make-string (max 1 (- 15 (length (cdr (assoc 'key define))))) ? )
                            " = "
                            (propertize (cdr (assoc 'value define)) 'face font-lock-string-face)
                            "\n")))
              defines)))

(defun cdn-context-print-group (selection group)
  (let (elems (i-elem 0) i-level epxansions prefix i-expansion (output ""))
    (setq elems (cdr (assoc group selection)))
    (setq i-elem 0)
    (mapc (lambda (elem)
            (setq i-elem (+ i-elem 1))
            (setq expansions (cdr (assoc 'expansions elem)))
            (setq output (concat output
                                 "\n"
                                 (propertize (concat (capitalize (symbol-name group))
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
    (insert (propertize output 'invisible group))))


(defun cdn-context-button (name other1 other2 hide-list msg)
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] `(lambda ()
                                      (interactive)
                                      (with-current-buffer "CDN context"
                                        (setq buffer-invisibility-spec '(,@hide-list))
                                        (overlay-put (symbol-value (intern (concat ,name "-button"))) 'face '(:weight bold))
                                        (overlay-put (symbol-value (intern (concat ,other1 "-button"))) 'face '(:weight normal))
                                        (overlay-put (symbol-value (intern (concat ,other2 "-button"))) 'face '(:weight normal)))))
    (insert (propertize (capitalize name) 'keymap map 'mouse-face 'highlight 'help-echo msg)))
  (set (make-local-variable (intern (concat name "-button"))) (make-overlay (- (point) (length name)) (point))))

(defun cdn-context-show (cdn-file)
  ;; (if (and (eq major-mode 'cdn-mode) cdn-context-do-show)
  (if cdn-context-do-show
      (let ((buffer (get-file-buffer cdn-file))
            (data (elt (cdr (assoc 'data cdn-context)) 0))
            selections (i-selection 0) title)
        (if (equal (file-truename cdn-file) (file-truename (cdr (assoc 'filename data))))
            (save-selected-window
              (switch-to-buffer-other-window "CDN context")
              (local-set-key "q" (lambda () (interactive) (kill-buffer-and-window)))
              (erase-buffer)
              (setq buffer-invisibility-spec ())
              (setq title (concat "CDN context for " (file-name-nondirectory cdn-file)))
              (insert (propertize (concat title "\n" (make-string (length title) ?-) "\n") 'face font-lock-comment-face))
              (setq selections (cdr (assoc 'selections data)))
              (cdn-context-print-defines selections)
              (insert (propertize "\n\nShow selections:\t" 'face '(:slant italic)))
              (cdn-context-button "in" "out" "both" '(out) "Click here to hide Out elements")
              (insert " ")
              (cdn-context-button "out" "in" "both" '(in) "Click here to hide In elements")
              (insert " ")
              (cdn-context-button "both" "in" "out" '() "Click here to show all elements")
              (overlay-put both-button 'face '(:weight bold))
              (insert "\n")
              (mapc (lambda (selection)
                      (setq i-selection (+ i-selection 1))
                      (insert (propertize (concat "\nSelection " (number-to-string i-selection) "\n") 'face '(:inherit font-lock-type-face :weight bold)))
                      (cdn-context-print-group selection 'in)
                      (cdn-context-print-group selection 'out))
                    selections)
              (goto-char (point-min))))))
  (setq cdn-context-do-show nil))


(defun cdn-context-update ()
  "Runs cdn-context, parse JSON, check error"
  ;; (if (and (eq major-mode 'cdn-mode) (not cdn-context-updating))
  (if (and (eq major-mode 'cdn-mode) (file-readable-p buffer-file-name))
      (let (json-buf proc)
        ;; (setq cdn-context-updating t)
        (setq json-buf (generate-new-buffer (concat (buffer-name) " cdn-context")))
        (setq proc (start-process "cdn-context" json-buf "cdn-context" (file-name-nondirectory (buffer-file-name))
                                  "-l" (number-to-string (line-number-at-pos))
                                  "-c" (number-to-string (+ (current-column) 1))))
        (set-process-sentinel proc 'cdn-context-parse))))

(defun cdn-context-check ()
  "Check the CDN file syntax"
  (interactive)
  (if (eq major-mode 'cdn-mode)
      (progn
        (setq cdn-context-do-show (called-interactively-p))
        (if (buffer-modified-p)
            (save-buffer) ; will trigger cdn-context-update
          (cdn-context-update)))))

(defun cdn-context-clear (start stop)
  "Clears the cdn-context error face"
  (if (and (stringp (this-command-keys)) (eq major-mode 'cdn-mode))
      (delete-overlay cdn-context-overlay)))

(defvar cdn-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.cdn\\'" . cdn-mode))

(defvar cdn-font-lock-keywords
  (list
   '("\\(\\(\\w\\|_\\)+\\)}?[ \t]*\\(\\?=\\|<=\\|=\\)" 1 font-lock-variable-name-face)
   '("@\\([0-9]+\\|[a-zA-Z_]\\(\\w\\|_\\)*\\)?\\(\\[[^[]*\\(\\[[^[]*\\]\\)?[^[]*\\]\\)*" . font-lock-constant-face)
   '("\\_<\\(defines\\|or\\|network\\|templates\\|functions\\|integrator\\|interface\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(group\\|state\\|link\\|on\\|from\\|to\\|all\\|input-file\\|when-applied\\|when-unapplied\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(polynomial\\|piece\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(layout\\|left-of\\|right-of\\|above\\|below\\|at\\|of\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(delete\\|apply\\|unapply\\|property\\|action\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(integrated\\|in\\|out\\|once\\)\\_>" . font-lock-type-face)
   '("\\_<\\(debug\\|debug-print\\|context\\|selector\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(import\\|as\\|include\\|apply\\|unapply\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(root\\|templates-root\\|self\\|states\\|groups\\|links\\|properties\\|objects\\|count\\|if\\|is-empty\\|remove\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(has-tag\\|has-flag\\|has-template\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(children\\|parent\\|descendants\\|ancestors\\|first-child\\|last-child\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(first\\|last\\|subset\\|siblings\\|type\\|name\\|unique\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(bidirectional\\|proxy\\|each\\|no-self\\|probability\\|tag\\)\\_>" . font-lock-type-face)
   )
  "Highlighting for cdn mode")

(defun previous-non-empty-line ()
  "Moves to the first previous non-empty line"
  (interactive)
  (while (progn (forward-line -1)
                (looking-at "^[ \t]*$"))))

(defun cdn-indent-line ()
  "Indent current line as CDN code"
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

(defvar cdn-mode-syntax-table
  (let ((cdn-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# ".12" cdn-mode-syntax-table)
    (modify-syntax-entry ?# "<" cdn-mode-syntax-table)
    (modify-syntax-entry ?\n ">" cdn-mode-syntax-table)
    (modify-syntax-entry ?\n ">b" cdn-mode-syntax-table)
    (modify-syntax-entry ?- "_" cdn-mode-syntax-table)
    (modify-syntax-entry ?+ "." cdn-mode-syntax-table)
    (modify-syntax-entry ?/ "." cdn-mode-syntax-table)
    (modify-syntax-entry ?* "." cdn-mode-syntax-table)
    (modify-syntax-entry ?= "." cdn-mode-syntax-table)
    (modify-syntax-entry ?< "." cdn-mode-syntax-table)
    (modify-syntax-entry ?> "." cdn-mode-syntax-table)
    (modify-syntax-entry ?| "." cdn-mode-syntax-table)
    cdn-mode-syntax-table)
  "Syntax table for the cdn-mode")

(defun cdn-mode ()
  "Major mode for editing CDN files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table cdn-mode-syntax-table)
  (use-local-map cdn-mode-map)
  (set (make-local-variable 'cdn-context-overlay) (make-overlay 0 0))
  (overlay-put cdn-context-overlay 'face font-lock-warning-face)
  (set (make-local-variable 'font-lock-defaults) '(cdn-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'cdn-indent-line)
  (setq major-mode 'cdn-mode)
  (setq mode-name "CDN")
  (setq comment-start "#")
  (set (make-local-variable 'cdn-context) nil)
  (set (make-local-variable 'cdn-context-do-show) nil)
  ;; (set (make-local-variable 'cdn-context-updating) nil)
  (make-local-variable 'before-change-functions)
  (add-to-list 'before-change-functions 'cdn-context-clear)
  (add-hook 'after-save-hook 'cdn-context-update)
  (run-hooks 'cdn-mode-hook)
  (cdn-context-update))

(provide 'cdn-mode)
