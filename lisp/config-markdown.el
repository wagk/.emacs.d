;;; Markdown-related helpers
;; We rely on Markdown as our store of information. `obsidian-mode' can feel a
;; bit sus at times and we don't use all of its features anyway. Have a small
;; bootstrapped markdown configuration we can use.
(require 'config-evil)
(require 'config-helpers)

(use-package markdown-mode
  :ensure (:host github :repo "jrblevin/markdown-mode")
  :mode ("\\.md\\'" . markdown-mode)
  :after config-theme
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
  :custom-face
  (markdown-header-face
   ((default . (:inherit sol-superstrong-foreground))))
  (markdown-header-face-1
   ((default . (:inherit markdown-header-face))))
  (markdown-header-face-2
   ((default . (:inherit markdown-header-face))))
  (markdown-header-face-3
   ((default . (:inherit markdown-header-face))))
  (markdown-header-face-4
   ((default . (:inherit markdown-header-face))))
  (markdown-header-face-5
   ((default . (:inherit markdown-header-face))))
  (markdown-header-face-6
   ((default . (:inherit markdown-header-face))))
  (markdown-header-delimiter-face
   ((default . (:inherit markdown-header-face))))
  (markdown-metadata-key-face
   ((default . (:inherit sol-strong-foreground
                :weight semi-light))))
  (markdown-metadata-value-face
   ((default . (:inherit sol-foreground
                :weight semi-light))))
  (markdown-highlighting-face
   ((default . (:box t))))
  (markdown-strike-through-face
   ((default . (:strike-through t
                :inherit sol-light-foreground))))
  (markdown-inline-code-face
   ((default . (:inherit (sol-foreground
                          sol-superlight-background)))))
  (markdown-code-face
   ((default . (:inherit markdown-inline-code-face
                :extend t))))
  (markdown-table-face
   ((default . (:inherit sol-light-foreground))))
  (markdown-reference-face
   ((default . (:inherit sol-light-foreground))))
  (markdown-italic-face
   ((default . (:italic t))))
  (markdown-bold-face
   ((default . (:bold t))))
  (markdown-plain-url-face
   ((default . (:inherit markdown-italic-face))))
  (markdown-url-face
   ((default . (:inherit (markdown-italic-face
                          sol-light-foreground)))))
  (markdown-link-face
   ((default . (:inherit markdown-italic-face))))
  (markdown-blockquote-face
   ((default . (:inherit sol-light-foreground))))
  :general
  (markdown-mode-map
   :states '(normal insert)
    "<tab>" #'--markdown-complete-or-indent-at-table
    "TAB" "<tab>")
  :init
  (defface --markdown-date-timestamp-face
    '((default . (:weight semi-light)))
    "Face used to describe date timestamps. Like years and months and days."
    :group 'personal)
  (defconst --markdown-date-timestamp-regex
    (rx (= 4 (any digit)) "-" (any "0-1") (any digit) "-" (any "0-3") (any digit)))

  (defface --markdown-time-timestamp-face
    '((default . (:weight semi-light)))
    "Face used to describe time timestamps. Like hours and minutes."
    :group 'personal)
  (defconst --markdown-time-timestamp-regex
    (rx (= 2 (any digit)) ":" (= 2 (any digit))
        (* ":" (= 2 (any digit)))
        (* " +" (= 4 (any digit)))))

  (defface --markdown-tag-face
    '((default . (:weight light :inherit sol-light-foreground)))
    "Face used to describe tags (like `#foo'). I like using tags."
    :group 'personal)
  (defconst --markdown-tag-keyword-regex
    (rx (or line-start space punct) "#" (one-or-more (any alnum "_" "-"))))

  (setq initial-major-mode 'markdown-mode)
  (cl-defun --markdown-complete-or-indent-at-table ()
    (interactive)
    (if (markdown-table-at-point-p)
        (call-interactively #'markdown-table-forward-cell)
      (indent-for-tab-command)))

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
        "# " (->> (file-name-base (buffer-file-name))
                  (s-replace "-" " "))
        "\n"
        "\n")))

  ;; (with-eval-after-load 'evil
  ;;   (evil-define-text-object --markdown-code-block))

  ;; (with-eval-after-load 'smartparens
  ;;   ;; (sp-local-pair 'markdown-mode "`" nil :when '(:add))
  ;;   (sp-local-pair 'markdown-mode "```" "```"
  ;;                  :post-handlers '((--double-newline-and-indent-braces "RET"))))

  ;; (with-eval-after-load 'aggressive-fill-paragraph
  ;;   (add-hook 'markdown-mode-hook #'aggressive-fill-paragraph-mode))

  ;; (with-eval-after-load 'visual-fill-column
  ;;   (add-hook 'markdown-mode-hook #'visual-line-fill-column-mode))

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
  (font-lock-add-keywords
   'markdown-mode `((,--markdown-tag-keyword-regex 0 '--markdown-tag-face)
                    (,--markdown-date-timestamp-regex 0 '--markdown-date-timestamp-face)
                    (,--markdown-time-timestamp-regex 0 '--markdown-time-timestamp-face)))
  (font-lock-add-keywords
   'gfm-mode `((,--markdown-tag-keyword-regex 0 '--markdown-tag-face)
               (,--markdown-date-timestamp-regex 0 '--markdown-date-timestamp-face)
               (,--markdown-time-timestamp-regex 0 '--markdown-time-timestamp-face))))
  ;; :config
  ;; (define-advice markdown-toggle-gfm-checkbox
  ;;     (:after () insert-checkbox-if-none)
  ;;   "Inserts a checkbox if there is none there"
  ;;   (markdown-insert-gfm-checkbox)))

(use-package edit-indirect
  :after markdown-mode)

(use-package evil-markdown
  :after markdown-mode
  :ensure (:host github :repo "Somelauw/evil-markdown")
  :commands (evil-markdown-mode)
  :blackout evil-markdown-mode
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

(defvar config-markdown-active-vault nil
  "Last selected vault. Intended for transient use.")

(defconst config-markdown-time-capture-format-string "-- %F %H:%M:%S %z --")

(defconst config-markdown-checkbox-regex "- \\[ \\]"
  "These are rg regexes")
(defconst config-markdown-checkbox-done-regex "- \\[[xX]\\]"
  "These are rg regexes")

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

(cl-defun config-markdown-select-directory ()
  "Select a directory from `config-markdown-directories'.
If there is only one directory just return that."
  (interactive)
  (let ((dir (--completing-read (format "Directory (active: %s): "
                                        (or config-markdown-active-vault "none"))
                                config-markdown-directories
                                :require-match t)))
    (setq config-markdown-active-vault dir)
    dir))

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
  (if-let* ((raw-frontmatter (with-temp-buffer
                               (if (file-exists-p candidate)
                                   (insert-file-contents-literally candidate)
                                 (message "Candidate %s does not exist" candidate)
                                 nil)
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
            (marginalia--fields
             (summary :face 'bold)
             (tags :face '--markdown-tag-face)
             (aliases :face '--markdown-tag-face))
          ;; TODO: `propertize' somehow isn't propagating here, but is by
          ;; `marginalia--fields'.
          (concat
           (unless (string-empty-p summary)
             (concat summary))
           " " tags " " aliases)))
    " ~"))

(with-eval-after-load 'marginalia
  (cl-defun config-markdown-marginalia-annotator (candidate)
    "Opens a file and reads the metadata"
    (require 'dash)
    (require 'marginalia nil :noerror)
    (if-let* ((candidate (file-name-concat config-markdown-active-vault candidate))
              (raw-frontmatter (with-temp-buffer
                                 (if (file-exists-p candidate)
                                     (insert-file-contents-literally candidate)
                                   (message "Candidate %s does not exist" candidate)
                                   nil)
                                 (config-markdown-get-yaml-front-matter)))
              (frontmatter (condition-case err
                               (yaml-parse-string raw-frontmatter
                                                  :null-object nil)
                             (t
                              (message "Error annotating %s." candidate)
                              nil))))
        (let ((summary (--> frontmatter
                            (gethash 'summary it)
                            (or it "~")))
              ;; Format tags by sticking `#' in front of all of them
              (tags (--> frontmatter
                         (gethash 'tags it)
                         (mapcar #'(lambda (h)
                                     (->> h
                                          (string-replace " " "-")
                                          (downcase)
                                          (concat "#"))) it)))
              ;; Format aliases by sticking `&' in front of all of them
              (aliases (--> frontmatter
                            (gethash 'aliases it)
                            (mapcar #'(lambda (a)
                                        (->> a
                                             (string-replace " " "-")
                                             (downcase)
                                             (concat "&"))) it))))
          (marginalia--fields
           (tags :width 30 :face 'success)
           (aliases :width 30 :face 'match)
           (summary :width 30 :face 'bold)))
      " ~"))
  (add-to-list 'marginalia-prompt-categories
               '("\\<markdown\\>" . markdown))
  (add-to-list 'marginalia-annotator-registry
               '(markdown config-markdown-marginalia-annotator none)))

(cl-defun config-markdown-select-file-name (vault)
  "Search `config-markdown-directories' for files ending in `.md'.
DEFAULT-VAULT should be an element of `config-markdown-directories', but it is
not currently enforced."
  (interactive (list (config-markdown-select-directory)))
  (cl-assert vault)
  (require 'ht)
  (let* ((dir vault)
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
                (if-let* ((annot (ht-get memo cand)))
                    annot
                  (let ((text (config-markdown--select-file-annotation-function
                               (file-name-concat dir cand))))
                    (ht-set memo cand text)
                    text)))))
         ;; note that `Markdown' here is something marginalia tracks
         (file (--completing-read (format "Markdown File [%s]: " dir) files)))
    (unless (file-name-extension file)
      (setq file (file-name-with-extension file "md")))
    (file-name-concat dir file)))

(cl-defun config-markdown--find-files-named (name &optional dir)
  "Completing-read for a name of a file, relative to DIR.
If there are multiple files, completing-read for one of them."
  (interactive)
  (let* ((dir (or dir (config-markdown-select-directory)))
         (case-fold-search t)
         (files (mapcar (lambda (file)
                          (file-relative-name file dir))
                        (directory-files-recursively dir name)))
         (diary (--completing-read (format "%s: " name) files
                                   :require-match t)))
    (file-name-concat dir diary)))

(cl-defun config-markdown-find-file (&optional vault)
  "Opens a markdown file in `config-markdown-directories'."
  (interactive)
  (require 'dash)
  (-> (or vault (config-markdown-select-directory))
      (config-markdown-select-file-name)
      (find-file)))

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
  (let* ((file (config-markdown-select-file-name
                (if-let* ((buffer-name (buffer-file-name))
                          (vault-name (config-markdown-file-vault buffer-name)))
                    vault-name
                  (or config-markdown-active-vault
                      (config-markdown-select-directory)))))
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

(cl-defun config-markdown--find-file-from-jira-ticket (vault))

(with-eval-after-load 'transient
  ;; (transient-define-infix --selected-markdown-vault ()
  ;;   :description "Default markdown vault"
  ;;   :class 'transient-option
  ;;   ;; :class 'transient-lisp-variable
  ;;   ;; :variable 'config-markdown-active-vault
  ;;   :argument "--vault="
  ;;   :key "v"
  ;;   ;; :unsavable nil
  ;;   ;; :always-read nil
  ;;   :allow-empty nil
  ;;   ;; used for `transient-history'
  ;;   ;; :history-key 'config-markdown-active-vault
  ;;   :reader
  ;;   (lambda (_prompt _initial-input _history)
  ;;     (config-markdown-select-directory))
  ;;   :init-value
  ;;   (lambda (obj)
  ;;     (oset obj value (or config-markdown-active-vault
  ;;                         (car config-markdown-directories)))))

  (transient-define-infix --set-active-vault ()
    :class 'transient-lisp-variable
    :key "v"
    :description "Active Vault:"
    :variable 'config-markdown-active-vault
    :allow-empty nil
    :init-value
    (lambda (this)
      (unless config-markdown-active-vault
        (setq config-markdown-active-vault
              (car config-markdown-directories)))
      (oset this value config-markdown-active-vault))
    :reader
    (lambda (_prompt _initial-input _history)
      ;; we internally track the variable instead of doing it via transient
      ;; transient doesn't do anything here actually.
      (config-markdown-select-directory)))

  ;; (transient-define-suffix --markdown-find-file-from-transient ()
  ;;   "Should strongly consider refactoring things here now."
  ;;   (interactive)
  ;;   (let ((vault (transient-arg-value
  ;;                 "--vault="
  ;;                 (transient-args transient-current-command))))
  ;;     (config-markdown-find-file vault)))

  (transient-define-prefix --my-markdown-do ()
    "Transient organizing all the interesting markdown PKB commands."
    ["Configuration"
      (--set-active-vault)]
    [:description "Personal Knowledge Base Commands\n"
     ["Find (in folder)"
      ("f f" "File"
       (lambda () (interactive)
         (config-markdown-find-file config-markdown-active-vault)))
      ("f i" "File heading"
       (lambda () (interactive)
         (config-markdown-find-file
          config-markdown-active-vault)
         (consult-imenu)))
      ("f a" "File diary"
       (lambda () (interactive)
         (assert config-markdown-directories
                 t "markdown notes directory not set!")
         (find-file (config-markdown--find-files-named
                     "Diary"
                     config-markdown-active-vault))))]
     ["Insert"
      ("i i" "Insert link to file" config-markdown-insert-link-to-vault-file)
      ("i d" "Insert datetree entry in file"
       (lambda () (interactive)
         (end-of-buffer)
         (insert (markdown-datetree-template-heading))
         (insert (format-time-string
                  (concat "\n" config-markdown-time-capture-format-string "\n")))
         (command-execute #'evil-insert)))]]
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
      ("h f" "into file header"
       (lambda () (interactive)
         (require 'org-capture)
         (require 'config-org-capture)
         (org-capture nil "fh")))]]
    ["Grep"
     ["Anything"
      ("r r" "Anything in project"
       (lambda () (interactive)
         (require 'rg)
         ;; copied from macroexpanded `rg-define-search'
         (consult-ripgrep
           (or config-markdown-active-vault
                       (config-markdown-select-directory)))))]
         ;; (rg-run (rg-read-pattern nil)
         ;;         "everything"
         ;;         (or config-markdown-active-vault
         ;;             (config-markdown-select-directory))
         ;;         :literal)))]
     ["TODOs"
      ("t t" "In this file"
       (lambda () (interactive)
         (require 'rg)
         (rg-run config-markdown-checkbox-regex (file-relative-name
                                                 (buffer-file-name))
                 default-directory))
       :if (lambda ()
             (and (buffer-file-name)
                  (or (eq major-mode 'gfm-mode)
                      (eq major-mode 'markdown-mode)))))
      ("t f" "In file"
       (lambda () (interactive)
         (require 'rg)
         (let* ((dir (or config-markdown-active-vault
                         (config-markdown-select-directory)))
                (file (file-relative-name
                       (config-markdown-select-file-name dir)
                       dir)))
           (rg-run config-markdown-checkbox-regex file dir))))
      ("t a" "Any To Dos in project"
       (lambda () (interactive)
         (require 'rg)
         (rg-run config-markdown-checkbox-regex
                 "everything" (or config-markdown-active-vault
                                  (config-markdown-select-directory)))))]
     ["DONEs"
      ("T t" "In this file"
       (lambda () (interactive)
         (require 'rg)
         (rg-run config-markdown-checkbox-done-regex (file-relative-name
                                                      (buffer-file-name))
                 default-directory))
       :if (lambda ()
             (and (buffer-file-name)
                  (or (eq major-mode 'gfm-mode)
                      (eq major-mode 'markdown-mode)))))
      ("T f" "In file"
       (lambda () (interactive)
         (require 'rg)
         (let* ((dir (or config-markdown-active-vault
                         (config-markdown-select-directory)))
                (file (file-relative-name
                       (config-markdown-select-file-name dir)
                       dir)))
           (rg-run config-markdown-checkbox-done-regex
                   file dir))))
      ("T a" "Any To Dos in project"
       (lambda () (interactive)
         (require 'rg)
         (rg-run config-markdown-checkbox-done-regex
                 "everything" (or config-markdown-active-vault
                                  (config-markdown-select-directory)))))]]))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "nn" #'(lambda () (interactive)
                                 (require 'transient)
                                 (--my-markdown-do)))
  (--evil-define-splits "nf" #'(lambda () (interactive)
                                 (config-markdown-find-file
                                  config-markdown-active-vault)))
  (--evil-define-splits "nfi" #'(lambda () (interactive)
                                  (config-markdown-find-file
                                   config-markdown-active-vault)
                                  (consult-imenu)))
  (evil-ex-define-cmd "ni" #'config-markdown-insert-link-to-vault-file)
  (with-eval-after-load 'magit
    ;; (cl-defun --magit-dispatch-vault ()
    ;;   ;; not working!
    ;;   "Open `magit-dispatch' for current active vault."
    ;;   (interactive)
    ;;   (require 'magit)
    ;;   (let ((default-directory config-markdown-active-vault))
    ;;     (message "default directory is %s" default-directory)
    ;;     (magit-dispatch)))
    (--evil-define-splits "ng" #'(lambda () (interactive)
                                   (dired config-markdown-active-vault)))))

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
             :timestamp-format config-markdown-time-capture-format-string
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
            ;; :function config-markdown--find-file-and-point
            :function
            ,#'(lambda () (interactive)
                 (config-markdown--find-file-and-point
                  (config-markdown-find-file config-markdown-active-vault)))
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
                 (config-markdown-find-file config-markdown-active-vault)
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
                 (find-file (config-markdown--find-files-named
                             "Diary"
                             config-markdown-active-vault))
                 (markdown-datetree-find-instant)
                 (outline-next-preface))
            :after-finalize
            ,#'--HACK-discard-last-stored-marker
            :template
            ,#'--datetree-capture-template)))))
(with-eval-after-load 'evil
  (evil-ex-define-cmd "nv" #'config-markdown-select-directory)
  (evil-ex-define-cmd "nd" #'(lambda () (interactive)
                               (require 'org-capture)
                               (require 'config-org-capture)
                               (assert config-markdown-directories
                                       t "markdown notes directory not set!")
                               (find-file (config-markdown--find-files-named
                                           "Diary"
                                           config-markdown-active-vault))))
  (evil-ex-define-cmd "ndd" #'(lambda () (interactive)
                                 (require 'org-capture)
                                 (require 'config-org-capture)
                                 (org-capture nil "ddd")))
  (evil-ex-define-cmd "nff" #'(lambda () (interactive)
                                (unless (or (eq major-mode 'gfm-mode)
                                            (eq major-mode 'markdown-mode))
                                  (user-error "Not in a markdown file!"))
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

;; (with-eval-after-load 'rg
;;   (rg-define-search config-markdown-search-in-notes
;;     :query point
;;     :files "everything"
;;     :dir (config-markdown-select-directory))
;;   (evil-ex-define-cmd "nr" 'config-markdown-search-in-notes))

(with-eval-after-load 'consult
  (cl-defun --consult-ripgrep-active-vault ()
    (interactive)
    (consult-ripgrep config-markdown-active-vault))
  (evil-ex-define-cmd "nr" #'--consult-ripgrep-active-vault))

;; Personal notes and the like
(use-package consult-notes
  :after markdown-mode
  :custom
  (consult-notes-default-format 'markdown-mode)
  (consult-notes-file-dir-sources
   `(("obsidian" ?o ,config-markdown-active-vault))))

;; (use-package consult-notes
;;   :disabled t
;;   :after (config-evil-helpers consult)
;;   :if (bound-and-true-p --notes-folder)
;;   :commands
;;   (consult-notes
;;    consult-notes-search-in-all-notes)
;;   :custom
;;   (consult-notes-file-dir-sources
;;    `(("Deft" ?d ,--notes-folder)))
;;   :init
;;   (--evil-ex-define-cmds-splits-and-tabs
;;    "nn"
;;    #'consult-notes
;;    #'(lambda () (consult-notes))))

(use-package obsidian
  :disabled t ;; sync timer is killing us
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
  (when obsidian-directory
    (global-obsidian-mode))
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
