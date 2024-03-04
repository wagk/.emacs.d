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

(defcustom config-markdown-directory nil
  "Location of Markdown directory."
  :type (list directory))

(cl-defun config-markdown--select-file-name ()
  "Search `config-markdown-directory' for files ending in `.md'."
  (interactive)
  (require 'dash)
  (let* ((files (mapcar
                 (lambda (dir)
                   (directory-files-recursively dir "\\.md$"))
                 config-markdown-directory)))
    (--completing-read "File: " (apply #'append files))))

(cl-defun config-markdown-find-file ()
  "Opens a markdown file in `config-markdown-directory'."
  (interactive)
  (find-file (config-markdown--select-file-name)))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "nn" #'config-markdown-find-file))

(cl-defun config-markdown--find-heading-end-point ()
  "Assuming that we are in the appropriate capture file, find the capture
point."
  ;; we want to append, so go to the next outline and backtrack
  (pcase (markdown-outline-next)
    ('nil (goto-char (point-max)))
    (_ (beginning-of-line)
       (newline-and-indent)
       (forward-line -1))))

(cl-defun config-markdown--find-or-insert-date-heading-point ()
  "Find heading containing today's date, and go to the bottom of it."
  (let* ((today (format-time-string "%F"))
         (regex (rx (1+ "#") " " (literal today))))
    (goto-char (point-min))
    (unless (search-forward-regexp regex nil :move-to-end)
      ;; Top level heading
      ;; TODO (pangt): Make this dynamic
      (markdown-insert-header 1 today))))

(cl-defun config-markdown--find-file-and-point ()
  (config-markdown-find-file)
  (condition-case err
      (consult-imenu)
    ;; no headings in file
    (t (goto-char (point-max)))
    (:success
      ;; ignore list items
      (if (markdown-list-item-at-point-p)
          (goto-char (point-max))
        (config-markdown--find-heading-end-point)))))

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
                 (assert config-markdown-directory
                         t "markdown notes directory not set!")
                 (-> config-markdown-directory
                     (file-name-concat "Diary.md")
                     (find-file))
                 (config-markdown--find-or-insert-date-heading-point))
            :unnarrowed t
            :empty-lines 1
            :append t
            :after-finalize
            ,#'(lambda () (setq org-capture-last-stored-marker (make-marker)))
            :template "%?")))))

(with-eval-after-load 'rg
  (rg-define-search config-markdown-search-in-notes
    :files "everything"
    :dir config-markdown-directory)
  (evil-ex-define-cmd "ng" 'config-markdown-search-in-notes))

;;; Experimentations with datetrees in markdown

(provide 'config-markdown)
