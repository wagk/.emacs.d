;;; Markdown-related helpers
;; We rely on Markdown as our store of information. `obsidian-mode' can feel a
;; bit sus at times and we don't use all of its features anyway. Have a small
;; bootstrapped markdown configuration we can use.
(require 'config-evil)

(use-package markdown-mode
  :ensure (:host github :repo "jrblevin/markdown-mode")
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode)
  :custom
  (markdown-asymmetric-header t)
  (markdown-code-block-braces nil)
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
    "<tab>" #'--markdown-complete-or-indent-at-table
    "TAB" "<tab>")
  :init
  (setq initial-major-mode 'gfm-mode)
  (cl-defun --markdown-complete-or-indent-at-table ()
    (interactive)
    (if (markdown-table-at-point-p)
        (call-interactively #'markdown-table-forward-cell)
      (indent-for-tab-command)))

  (cl-defun --read-word-list (&optional &key (prompt "Words")
                                        (func #'identity)
                                        (split-separator ",")
                                        (join-separator ", "))
    "Reads a comma-separted input list and normalizes it.
If FUNC is passed in then run func for each string in the list and concatenate
the results."
    (interactive)
    (require 's)
    (--> (read-string (format "%s [optional, \"%s\"-separated]: "
                              prompt split-separator))
        (s-split split-separator it :omit-nulls)
        (mapcar #'s-trim it)
        (mapcar (-partial #'s-replace " " "-") it)
        (mapcar #'s-downcase it)
        (mapcar func it)
        (s-join join-separator it)))

  (cl-defun --read-tags ()
    (--read-word-list :prompt "Tags"
                      :join-separator " "
                      :func #'(lambda (s)
                                (require 's)
                                (if (s-prefix-p "#" s)
                                    s
                                  (concat "#" s)))))

  (with-eval-after-load 'autoinsert
    (define-auto-insert "\\.md\\'"
      '("Front matter"
        "---\n"
        "aliases: [" (--read-word-list :prompt "Aliases") "]\n"
        "created: " (format-time-string "%F") "\n"
        "summary: " (read-string "Summary [optional]: " nil nil "~") "\n"
        "tags: [" (--read-word-list :prompt "Tags") "]\n"
        "---\n"
        "\n"
        "# " (file-name-base (buffer-file-name)) "\n"
        "\n")))

  ;; (with-eval-after-load 'evil
  ;;   (evil-define-text-object --markdown-code-block))

  ;; (with-eval-after-load 'smartparens
  ;;   ;; (sp-local-pair 'markdown-mode "`" nil :when '(:add))
  ;;   (sp-local-pair 'markdown-mode "```" "```"
  ;;                  :post-handlers '((--double-newline-and-indent-braces "RET"))))

  ;; (with-eval-after-load 'aggressive-fill-paragraph
  ;;   (add-hook 'markdown-mode-hook #'aggressive-fill-paragraph-mode))

  (with-eval-after-load 'visual-fill-column
    (add-hook 'markdown-mode-hook #'visual-line-fill-column-mode))

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
    (cl-pushnew '("md" . gfm) org-src-lang-modes)))
  ;; :config
  ;; (define-advice markdown-toggle-gfm-checkbox
  ;;     (:after () insert-checkbox-if-none)
  ;;   "Inserts a checkbox if there is none there"
  ;;   (markdown-insert-gfm-checkbox)))

(use-package edit-indirect
  :after markdown-mode)

(use-package evil-markdown
  :after (general markdown-mode)
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
  "Location of Markdown directories."
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

;; Lifted from `obsidian-get-yaml-front-matter'
(cl-defun config-markdown-buffer-yaml-front-matter (s)
  "Find YAML front matter in S.
Returns a hash table of the contents if some. nil otherwise.

\(STRING\)"
  (interactive (list (buffer-string)))
  (require 'yaml)
  (require 'dash)
  (if (s-starts-with-p "---" s)
      (let* ((split (s-split-up-to "---" s 2))
             (looks-like-yaml-p (eq (length split) 3)))
        (if looks-like-yaml-p
            (->> split
                 (nth 1)
                 yaml-parse-string)))))

;; Lifted from `obsidian-get-yaml-front-matter'
(cl-defun config-markdown-get-yaml-front-matter ()
  "Return the text of the YAML front matter of the current buffer.
Return nil if the front matter does not exist, or incorrectly delineated by
'---'.  The front matter is required to be at the beginning of the file."
  (save-excursion
    (goto-char (point-min))
    (when-let ((startpoint (re-search-forward "\\(^---\\)" 4 t 1))
               (endpoint (re-search-forward "\\(^---\\)" nil t 1)))
      (buffer-substring-no-properties startpoint (- endpoint 3)))))

;; I want to stick this inside `completion-extra-properties'
;; TODO: Eventually swap to marginalia to look nicer
(cl-defun config-markdown--select-file-annotation-function (candidate)
  "Opens a file and reads the metadata"
  (require 'dash)
  (require 'marginalia nil :noerror)
  (if-let ((raw-frontmatter (with-temp-buffer
                              (if (file-exists-p candidate)
                                  (insert-file-contents-literally candidate)
                                "")
                              (config-markdown-get-yaml-front-matter)))
           (frontmatter (condition-case err
                            (yaml-parse-string raw-frontmatter
                                               :null-object nil)
                          (t
                           (message "Error annotating %s." candidate)
                           nil))))
      (let ((summary (--> frontmatter
                          (gethash 'summary it "")
                          (or it "")))
            ;; Format tags by sticking `#' in front of all of them
            (tags (--> frontmatter
                       (gethash 'tags it "")
                       (mapconcat #'(lambda (h)
                                      (->> h
                                           (string-replace " " "-")
                                           (downcase)
                                           (concat "#")))
                                  it " ")))
            ;; Format aliases by sticking `&' in front of all of them
            (aliases (--> frontmatter
                          (gethash 'aliases it "")
                          (mapconcat #'(lambda (a)
                                         (->> a
                                              (string-replace " " "-")
                                              (downcase)
                                              (concat "&")))
                                     it " "))))
        (if (fboundp 'marginalia--fields)
            (marginalia--fields (tags :face '--markdown-tag-face)
                                (aliases :face '--markdown-tag-face)
                                (summary :face 'italic))
          ;; TODO: `propertize' somehow isn't propagating here, but is by
          ;; `marginalia--fields'.
          (concat " " tags " " aliases
                  (unless (string-empty-p summary)
                    (concat "\n" summary)))))
    " ~"))

(cl-defun config-markdown--select-file-affixation-function (cand-list)
  ":affixation-function for file selection")

(cl-defun config-markdown--select-directory ()
  "Select a directory from `config-markdown-directories'.
If there is only one directory just return that."
  (interactive)
  (--completing-read "Directory: "
                     config-markdown-directories
                     :require-match t))

(cl-defun config-markdown--select-file-name (&optional default-vault)
  "Search `config-markdown-directories' for files ending in `.md'.
DEFAULT-VAULT should be an element of `config-markdown-directories', but it is
not currently enforced."
  (interactive)
  (require 'ht)
  (let* ((dir (or default-vault
                  (config-markdown--select-directory)))
         (files (mapcar #'(lambda (file)
                            (file-relative-name file dir))
                        (directory-files-recursively dir "\\.md$")))
         (memo (ht-create))
         (completion-extra-properties
          ;; (list :affixation-function
          ;;   #'(lambda (cands)
          ;;       (marginalia--affixate
          ;;        nil
          ;;        #'config-markdown--select-file-annotation-function
          ;;        (mapcar #'(lambda (cand)
          ;;                    (file-name-concat dir cand))
          ;;                cands)))))
          (list :annotation-function
            #'(lambda (cand)
                ;; do some very basic caching because it's slow
                (if-let ((annot (ht-get memo cand)))
                    annot
                  (let ((text (config-markdown--select-file-annotation-function
                               (file-name-concat dir cand))))
                    (ht-set memo cand text)
                    text)))))
         (file (--completing-read (format "File [%s]: " dir) files)))
    (unless (file-name-extension file)
      (setq file (file-name-with-extension file "md")))
    (file-name-concat dir file)))

(cl-defun config-markdown--find-files-named (name &optional dir)
  "Completing-read for a name of a file, relative to DIR.
If there are multiple files, completing-read for one of them."
  (interactive)
  (let* ((dir (or dir (config-markdown--select-directory)))
         (case-fold-search t)
         (files (mapcar (lambda (file)
                          (file-relative-name file dir))
                        (directory-files-recursively dir name)))
         (diary (--completing-read (format "%s: " name) files
                                   :require-match t)))
    (file-name-concat dir diary)))

(cl-defun config-markdown-find-file ()
  "Opens a markdown file in `config-markdown-directories'."
  (interactive)
  (find-file (config-markdown--select-file-name)))

(cl-defun config-markdown--find-heading-insertion-point (style)
  "Assuming that we are in the appropriate heading, move point to either the top
or bottom.

STYLE can be either `:append' or `:prepend'"
  (pcase style
    (:prepend
     (end-of-line)
     (newline-and-indent)
     (newline-and-indent))
    (:append
     (pcase (markdown-outline-next)
       ('nil (goto-char (point-max)))
       (_point (beginning-of-line)
               (newline-and-indent)
               (previous-line))))))

(cl-defun config-markdown--level-of-heading-at-point ()
  "Returns the level of the header.
If point is not at a header, go to previous header, and return that level."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (when (looking-at config-markdown-header-regexp)
      (length (match-string 1)))))

(cl-defun config-markdown--find-or-insert-date-heading-point-at-level (level)
  "Find heading containing today's date, and go to the bottom of it."
  (let* ((today (format-time-string "%F"))
         (regex (rx (1+ "#") " " (literal today))))
    (goto-char (point-min))
    (unless (search-forward-regexp regex nil :move-to-end)
      (markdown-insert-header level today)
      (newline-and-indent))
    (config-markdown--find-heading-insertion-point :append)))

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

(cl-defun config-markdown-file-vault (filepath)
  "Given a filepath, tries to find the vault it belongs to (inside
  `config-markdown-directories').

Returns nil if it belongs to no vault."
  (cl-assert filepath)
  (-find #'(lambda (vault) (f-ancestor-of-p vault filepath))
         config-markdown-directories))

(cl-defun config-markdown-insert-link-to-vault-file (&optional &key ref-link)
  "Insert a link to a file, relative to a vault folder as specified by
`config-markdown-directories'."
  (interactive)
  (require 'dash)
  (require 'markdown-mode)
  (let* ((file (config-markdown--select-file-name
                (when (buffer-file-name)
                  (config-markdown-file-vault (buffer-file-name)))))
         (file (file-relative-name file
                                   (-find #'(lambda (vault)
                                              (f-ancestor-of-p vault file))
                                          config-markdown-directories)))
         (link-text (read-string "Link text: " (file-name-base file)))
         ;; obsidian url-encodes the link, and I don't see a good reason to break
         ;; compatibility here.
         (url-encoded-file (url-encode-url file)))
    (if ref-link
        (markdown-insert-reference-link
         link-text
         (read-string "Link label: ")
         url-encoded-file)
      (insert (concat "[" link-text "]" "(" url-encoded-file ")")))))

(with-eval-after-load 'transient
  (transient-define-prefix --my-markdown-do ()
    "Transient organizing all the interesting markdown PKB commands."
    [:description "Personal Knowledge Base Commands\n"
     ["Find (in folder)"
      ("f f" "File" config-markdown-find-file)
      ("f i" "File heading"
       (lambda () (interactive)
         (config-markdown-find-file)
         (consult-imenu)))
      ("f a" "File diary"
       (lambda () (interactive)
         (assert config-markdown-directories
                 t "markdown notes directory not set!")
         (find-file (config-markdown--find-files-named "Diary"))))]
     ["Insert"
      ("i i" "Insert link to file" config-markdown-insert-link-to-vault-file)]]
    ["Capture"
     ["as Datetime"
      ("d d" "into current file"
       (lambda () (interactive)
         (require 'org-capture)
         (require 'config-org-capture)
         (org-capture nil "ddff"))
       :if (lambda ()
             (and (buffer-file-name)
                  (or (eq major-mode 'gfm-mode)
                      (eq major-mode 'markdown-mode)))))
      ("d f" "into a specific file"
       (lambda () (interactive)
         (require 'org-capture)
         (require 'config-org-capture)
         (org-capture nil "ddf")))
      ("d a" "into diary"
       (lambda () (interactive)
         (require 'org-capture)
         (require 'config-org-capture)
         (org-capture nil "ddd")))]
     ["Into Header"
      ("h f" "In file"
       (lambda () (interactive)
         (require 'org-capture)
         (require 'config-org-capture)
         (org-capture nil "fh")))]]
    ["Grep"
     ["Anything"
      ("g g" "In folder"
       (lambda () (interactive)
         (require 'rg)
         (command-execute #'config-markdown-search-in-notes)))]
     ["TODOs"
      ("t t" "In this file"
       (lambda () (interactive)
         (require 'rg)
         (rg-run "- [ ]" (file-relative-name
                          (buffer-file-name))
                 default-directory
                 :literal))
       :if (lambda ()
             (and (buffer-file-name)
                  (or (eq major-mode 'gfm-mode)
                      (eq major-mode 'markdown-mode)))))
      ("t f" "In file"
       (lambda () (interactive)
         (require 'rg)
         (let* ((dir (config-markdown--select-directory))
                (file (file-relative-name
                       (config-markdown--select-file-name dir)
                       dir)))
           (rg-run "- [ ]" file dir :literal))))
      ("t a" "In folder"
       (lambda () (interactive)
         (require 'rg)
         (rg-run "- [ ]" "everything" (config-markdown--select-directory)
                 :literal)))]]))

(with-eval-after-load 'config-evil-helpers
  (evil-ex-define-cmd "nn" #'(lambda () (interactive)
                               (require 'transient)
                               (--my-markdown-do)))
  (--evil-define-splits "nf" #'config-markdown-find-file)
  (--evil-define-splits "nfi" #'(lambda () (interactive)
                                  (config-markdown-find-file)
                                  (consult-imenu)))
  (evil-ex-define-cmd "ni" #'config-markdown-insert-link-to-vault-file))

(with-eval-after-load 'org-capture
  (require 'doct)
  (require 'config-org-capture)
  (require 'markdown-datetree)
  (cl-defun --datetree-heading ()
    "Goes to the capture destination and figures out which datetree headers it
should prepopulate."
    (with-current-buffer (org-capture-get :buffer)
      (save-restriction
        (widen)
        (markdown-datetree-template-heading))))
  (cl-defun --datetree-capture-template ()
    (concat "%(--datetree-heading)\n"
            (--capture-template-interesting
             :timestamp-format "%F %H:%M:%S %z"
             :collect-tags nil)))
  (setq org-capture-templates
        (doct-add-to
         org-capture-templates
         `(("Header - File"
            :keys "fh"
            :type plain
            :empty-lines-before 1
            :contexts
            ((:when config-markdown-directories))
            :function config-markdown--find-file-and-point
            :after-finalize
            ,#'--HACK-discard-last-stored-marker
            :template "%?")
           ("Datetree - Current File"
            :keys "ddff"
            :type plain
            :empty-lines-before 1
            :contexts
            ((:when config-markdown-directories)
             (:in-mode "gfm-mode")
             (:in-mode "markdown-mode"))
            :function
            ,#'(lambda ()
                 (markdown-datetree-find-instant)
                 (outline-next-preface))
            :after-finalize
            ,#'--HACK-discard-last-stored-marker
            :template
            ,#'--datetree-capture-template)
           ("Datetree - Select File"
            :keys "ddf"
            :type plain
            :empty-lines-before 1
            :contexts
            ((:when config-markdown-directories))
            :function
            ,#'(lambda ()
                 (interactive)
                 (config-markdown-find-file)
                 (markdown-datetree-find-instant)
                 (outline-next-preface))
            :after-finalize
            ,#'--HACK-discard-last-stored-marker
            :template
            ,#'--datetree-capture-template)
           ("Datetree - Diary"
            :keys "ddd"
            :type plain
            :empty-lines-before 1
            :contexts
            ((:when config-markdown-directories))
            :function
            ,#'(lambda ()
                 (interactive)
                 (assert config-markdown-directories
                         t "markdown notes directory not set!")
                 (find-file (config-markdown--find-files-named "Diary"))
                 (markdown-datetree-find-instant)
                 (outline-next-preface))
            :after-finalize
            ,#'--HACK-discard-last-stored-marker
            :template
            ,#'--datetree-capture-template)))))
(with-eval-after-load 'evil
  (evil-ex-define-cmd "nd" #'(lambda () (interactive)
                               (require 'org-capture)
                               (require 'config-org-capture)
                               (assert config-markdown-directories
                                       t "markdown notes directory not set!")
                               (find-file (config-markdown--find-files-named "Diary"))))
  (evil-ex-define-cmd "ndd" #'(lambda () (interactive)
                                 (require 'org-capture)
                                 (require 'config-org-capture)
                                 (org-capture nil "ddd")))
  (evil-ex-define-cmd "nff" #'(lambda () (interactive)
                                (require 'org-capture)
                                (require 'config-org-capture)
                                (org-capture nil "ddff")))
  (evil-ex-define-cmd "ndf" #'(lambda () (interactive)
                                (require 'org-capture)
                                (require 'config-org-capture)
                                (org-capture nil "ddf")))
  (evil-ex-define-cmd "nfd" "ndf"))
  ;; (evil-ex-define-cmd "nff" #'(lambda () (interactive)
  ;;                               (require 'org-capture)
  ;;                               (org-capture nil "fh"))))

(with-eval-after-load 'rg
  (rg-define-search config-markdown-search-in-notes
    :query point
    :files "everything"
    :dir (config-markdown--select-directory))
  (evil-ex-define-cmd "nr" 'config-markdown-search-in-notes))

;; Personal notes and the like
(use-package consult-notes
  :disabled t
  :after (config-evil-helpers consult)
  :if (bound-and-true-p --notes-folder)
  :commands
  (consult-notes
   consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Deft" ?d ,--notes-folder)))
  :init
  (--evil-ex-define-cmds-splits-and-tabs
   "nn"
   #'consult-notes
   #'(lambda () (consult-notes))))

(use-package obsidian
  :after evil
  :custom
  (obsidian-include-hidden-files nil)
  (obsidian-links-use-vault-path t)
  ;; :hook
  ;; (obsidian-mode-hook . (lambda ()
  ;;                         (when (featurep 'aggressive-fill-paragraph)
  ;;                           (aggressive-fill-paragraph-mode 0))
  ;;                         (require 'visual-fill-column)
  ;;                         (visual-line-fill-column-mode 1)))
  :config
  ;; don't forget to `obsidian-specify-path'
  (global-obsidian-mode)
  (cl-defun --obsidian-find-buffer ()
    (interactive)
    (cl-letf ((symbol-function 'find-file) (symbol-function 'find-file-noselect))
      (obsidian-jump))))
  ;; (--evil-ex-define-cmds-splits-and-tabs
  ;;  "nn"
  ;;  #'obsidian-jump
  ;;  #'(lambda () (obsidian-jump)))
  ;; (evil-ex-define-cmd "ni" #'obsidian-insert-link))
  ;; (with-eval-after-load 'org-capture
  ;;   (setq org-capture-templates
  ;;         (doct-add-to org-capture-templates
  ;;                      '("Obsidian append"
  ;;                        :keys "c"
  ;;                        :type plain
  ;;                        :function (lambda ()
  ;;                                    (let ((buffer(->> (obsidian-list-all-files)
  ;;                                                      (--completing-read "File: ")
  ;;                                                      (find-file-noselect))))
  ;;                                      (with-current-buffer buffer
  ;;                                        (goto-char (point-max)))))
  ;;                        :template "\n")))

(require 'markdown-datetree)

(provide 'config-markdown)
