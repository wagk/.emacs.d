;;; Markdown-related helpers
;; We rely on Markdown as our store of information. `obsidian-mode' can feel a
;; bit sus at times and we don't use all of its features anyway. Have a small
;; bootstrapped markdown configuration we can use.
(require 'config-evil)

(use-package markdown-mode
  :ensure (:host github :repo "jrblevin/markdown-mode")
  :mode ("\\.md\\'" . markdown-mode)
  :commands (markdown-mode)
  :custom
  (markdown-asymmetric-header t)
  (markdown-code-block-braces t)
  (markdown-enable-highlighting-syntax t)
  (markdown-enable-math nil)
  (markdown-fontify-code-blocks-natively t)
  (markdown-footnote-location 'subtree)
  (markdown-gfm-uppercase-checkbox nil)
  (markdown-hide-urls nil)
  (markdown-list-indent-width 2)
  (markdown-unordered-list-item-prefix "- ")
  (markdown-wiki-link-fontify-missing nil)
  (markdown-wiki-link-search-type 'project)
  :general
  (markdown-mode-map
   :states '(normal insert)
   "<tab>" #'completion-at-point
   "TAB" "<tab>")
  :init
  (with-eval-after-load 'autoinsert
    (define-auto-insert "\\.md\\'"
      '("Front matter"
        "---\n"
        "created: " (format-time-string "%F") "\n"
        "---\n"
        "\n"
        "# " (file-name-base (buffer-file-name)) "\n"
        "\n")))
  (with-eval-after-load 'smartparens
    (sp-local-pair 'markdown-mode "```" "```"
                   :post-handlers '((--double-newline-and-indent-braces "RET"))))
  (with-eval-after-load 'aggressive-fill-paragraph
    (add-hook 'markdown-mode-hook #'aggressive-fill-paragraph-mode))
  (with-eval-after-load 'org-table
    (defun orgtbl-to-gfm (table params)
      "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
      (let* ((alignment (mapconcat
                         (lambda (x) (if x "|--:" "|---"))
                         org-table-last-alignment ""))
             (params2 (list :splice t
                            :hline (concat alignment "|")
                            :lstart "| " :lend " |" :sep " | ")))
        (orgtbl-to-generic table (org-combine-plists params2 params)))))
  (with-eval-after-load 'org-src
    (cl-pushnew '("md" . gfm) org-src-lang-modes))
  :config
  (define-advice markdown-toggle-gfm-checkbox
      (:after () insert-checkbox-if-none)
    "Inserts a checkbox if there is none there"
    (markdown-insert-gfm-checkbox)))

(use-package edit-indirect
  :after markdown-mode)

(use-package evil-markdown
  :after (markdown-mode)
  :ensure (:host github :repo "Somelauw/evil-markdown")
  :commands (evil-markdown-mode)
  :hook (markdown-mode-hook . evil-markdown-mode)
  :config
  (general-define-key
   :keymaps 'evil-markdown-mode-map
   :states 'insert
   "M-J" 'markdown-move-subtree-down
   "M-K" 'markdown-move-subtree-up
   "M-h" 'markdown-promote
   "M-j" 'markdown-move-down
   "M-k" 'markdown-move-up
   "M-l" 'markdown-demote))

(defgroup config-markdown nil
  "Personal Markdown hacks")

(defcustom config-markdown-directories nil
  "Location of Markdown directory."
  :type '(list directory))

(defconst config-markdown-header-regexp
  (rx bol
      (group-n 1 (one-or-more "#"))
      (one-or-more whitespace)
      (group-n 2 (one-or-more any))
      (zero-or-more whitespace)
      eol)
  "Matches the markdown header I use (the ones that start with \"#\").
Capture groups:
- 1: The headings. The length of this is the depth of the header.
- 2: The contents of the header.

Accounts for trailing whitespace.")

(cl-defun config-markdown--select-directory ()
  "Select a directory from `config-markdown-directories'.
If there is only one directory just return that."
  (interactive)
  (if (length= config-markdown-directories 1)
      (car config-markdown-directories)
    (--completing-read "Directory: " config-markdown-directories)))

(cl-defun config-markdown--select-file-name ()
  "Search `config-markdown-directories' for files ending in `.md'."
  (interactive)
  (require 'dash)
  (let* ((dir (config-markdown--select-directory))
         (files (mapcar (lambda (file)
                          (file-relative-name file dir))
                        (directory-files-recursively dir "\\.md$")))
         (file (--completing-read (format "File [%s]: " dir) files)))
    (unless (file-name-extension file)
      (setq file (file-name-with-extension file "md")))
    (file-name-concat dir file)))

(cl-defun config-markdown--find-files-named (name)
  (interactive)
  (let* ((dir (config-markdown--select-directory))
         (case-fold-search t)
         (files (mapcar (lambda (file)
                          (file-relative-name file dir))
                        (directory-files-recursively dir name)))
         (diary (if (length= files 1)
                    (car files)
                  (--completing-read (format "%s: " name) files))))
    (file-name-concat dir diary)))

(cl-defun config-markdown-find-file ()
  "Opens a markdown file in `config-markdown-directories'."
  (interactive)
  (find-file (config-markdown--select-file-name)))

(cl-defun config-markdown--find-heading-insertion-point (style)
  "Assuming that we are in the appropriate capture file, find the capture
point.

STYLE can be either `:append' or `:prepend'"
  ;; we want to append, so go to the next outline and backtrack
  (pcase style
    (:prepend
     (end-of-line)
     (newline-and-indent)
     (newline-and-indent))
    (:append
     (pcase (markdown-outline-next)
       ('nil (goto-char (point-max)))
       (_ (beginning-of-line)
          (newline-and-indent)
          (previous-line))))))

(cl-defun config-markdown--level-of-heading-at-point ()
  "Returns the level of the header.
If point is at a header, return the level, nil otherwise."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at config-markdown-header-regexp)
      (length (match-string 1)))))

(cl-defun config-markdown--predict-expected-level-of-heading-at-point ()
  "Look ahead and behind to guesstimate what should be the expected level.

| Before | After | Expected (at point) | Why                          |
|--------|-------|---------------------|------------------------------|
| #      | #     | #                   | Same level                   |
| #      | ##    | ##                  | Nested from now on           |
| ##     | #     | ##                  | Append into previous subtree |
")

(cl-defun config-markdown--find-or-insert-date-heading-point-at-level (level)
  "Find heading containing today's date, and go to the bottom of it."
  (let* ((today (format-time-string "%F"))
         (regex (rx (1+ "#") " " (literal today))))
    (goto-char (point-min))
    (unless (search-forward-regexp regex nil :move-to-end)
      ;; Top level heading
      ;; TODO (pangt): Make this dynamic
      (markdown-insert-header level today))))

(cl-defun config-markdown--find-file-and-point (&optional file)
  "Searches through files and headings and attempts to position the point at the
end of the selected heading."
  (interactive)
  (if file
      (find-file file)
    (config-markdown-find-file))
  (condition-case err
      (consult-imenu)
    ;; no headings in file
    (t (goto-char (point-max)))
    (:success
      ;; ignore list items
      (if (markdown-list-item-at-point-p)
          (goto-char (point-max))
        (config-markdown--find-heading-insertion-point :prepend)))))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "nn" #'config-markdown-find-file))

(with-eval-after-load 'org-capture
  (require 'doct)
  (setq org-capture-templates
        (doct-add-to
         org-capture-templates
         `(("Notes - File - Header"
            :keys "header"
            :type plain
            :function config-markdown--find-file-and-point
            :unnarrowed t
            :empty-lines 1
            :after-finalize
            ,#'(lambda () (setq org-capture-last-stored-marker (make-marker)))
            :template "%?")
           ("Notes - diary"
            :keys "diary"
            :type plain
            :unnarrowed t
            :function
            ,#'(lambda ()
                 (assert config-markdown-directories
                         t "markdown notes directory not set!")
                 (-> (config-markdown--find-files-named "Diary")
                     (find-file))
                 (let ((previous-heading-level
                        (save-excursion (markdown-previous-heading)
                                        (config-markdown--level-of-heading-at-point)))))
                 (config-markdown--find-or-insert-date-heading-point-at-level previous-heading-level))
            :unnarrowed t
            :empty-lines 1
            :append t
            :after-finalize
            ,#'(lambda () (setq org-capture-last-stored-marker (make-marker)))
            :template "%?")))))

(with-eval-after-load 'rg
  (rg-define-search config-markdown-search-in-notes
    :files "everything"
    :dir config-markdown-directories)
  (evil-ex-define-cmd "ng" 'config-markdown-search-in-notes))

;;; Experimentations with datetrees in markdown

(provide 'config-markdown)
