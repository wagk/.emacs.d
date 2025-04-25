(require 'use-package)
(require 'config-helpers)
(require 'config-project) ;; jira stuff

;; If magit complains about not finding the config on windows, it's
;; because of [this issue], the easiest solution is to make a link.
;;
;; `mklink %APPDATA%\.gitconfig %USERPROFILE%\.gitconfig`
;;
;; [this issue]: https://github.com/magit/magit/issues/1497
(use-package magit
  :ensure (:host github :repo "magit/magit" :branch "main")
  :after (evil config-theme)
  :defer 10
  :commands (magit
             magit-status
             magit-pull
             magit-commit)
  :custom
  (magit-prefer-remote-upstream t)
  (magit-blame-echo-style 'headings)
  (magit-blame-read-only t)
  (magit-log-auto-more t)
  (magit-log-show-refname-after-summary t)
  (magit-show-long-lines-warning nil)
  (magit-commit-ask-to-stage 'stage)
  :custom-face
  (magit-keyword ((default . (:foreground ,sol-cyan))))
  (magit-keyword-squash ((default . (:bold t
                                     :foreground ,sol-green))
                         (((type nil)) . (:bold nil))))
  (magit-header-line ((default . (:underline t
                                  :inherit magit-section-heading))
                      (((type nil)) . (:underline nil))))
  (magit-keyword-squash ((default . (:bold t
                                     :foreground ,sol-green))
                         (((type nil)) . (:bold nil))))
  (magit-head ((default . (:bold t
                           :foreground ,sol-magenta))
               (((type nil)) . (:bold nil))))
  (magit-branch-remote-head ((default . (:box nil
                                         :inverse-video t))))
  (magit-branch-current ((default . (:box nil
                                     :inverse-video t))))
  (magit-branch-local ((default . (:bold t
                                   :foreground ,sol-blue))
                       (((type nil)) . (:bold nil))))
  (magit-branch-remote ((default . (:bold t
                                    :foreground ,sol-green))
                        (((type nil)) . (:bold nil))))
  (magit-tag ((default . (:bold t
                          :foreground ,sol-violet))
              (((type nil)) . (:bold nil))))
  (magit-dimmed ((default . (:inherit sol-light-foreground
                             :foreground unspecified))))
  (magit-hash ((default . (:foreground unspecified
                           :bold nil
                           :inherit (sol-foreground)))
               (((type nil)) . (:bold nil))))
  (magit-cherry-equivalent ((default . (:foreground ,sol-magenta))))
  (magit-cherry-unmatched ((default . (:foreground ,sol-cyan))))
  :general
  ;; (general-define-key
  ;;   :keymaps 'project-prefix-map
  ;;   "v" 'magit-status)
  (magit-mode-map
   :states '(normal)
   "g x" 'magit-browse-thing)
  :init
  (cl-defun --magit-on-mode-save-buffers ()
    (save-some-buffers t #'save-some-buffers-root))

  ;; the *dispatch commands are probably the most important
  (evil-ex-define-cmd "gg"      'magit-dispatch)
  (evil-ex-define-cmd "ggg"     'magit-file-dispatch)
  (evil-ex-define-cmd "gf"      'magit-file-dispatch)

  (evil-ex-define-cmd "gB"      'magit-branch)
  (evil-ex-define-cmd "gd[iff]" 'magit-diff)
  (evil-ex-define-cmd "gb"      'magit-blame-addition)
  (evil-ex-define-cmd "blame"   'magit-blame-addition)
  (evil-ex-define-cmd "gblame"  'magit-blame)
  (evil-ex-define-cmd "gc"      'magit-commit)
  (evil-ex-define-cmd "git"     'ex-magit-cli)
  ;; (evil-ex-define-cmd "gl"      'magit-log)
  (evil-ex-define-cmd "glf"     'magit-log-buffer-file)
  (evil-ex-define-cmd "glb"    "glf")
  ;; (evil-ex-define-cmd "gll"     'magit-log-current)
  (evil-ex-define-cmd "gp"      'magit-push)
  (evil-ex-define-cmd "gz"      'magit-stash)
  (cl-defun --update-git-commit-comment-info ()
    "Markdown-mode sets the comments to HTML comments, but git commit messages
assume # starts a comment."
    (setq-local comment-start "#"
                comment-end ""))
  (cl-defun --prepare-git-commit-message ()
    "Prepopulates the commit message with stuff we want.
- Captures git subject prefixes [0], if any.
- Captures jira ticket from branch, if any.

[0]: https://kernelnewbies.org/PatchTipsAndTricks"
    (require 'dash)
    (if-let* (((--> (buffer-substring-no-properties
                     (point-min) (point-max))
                    (with-temp-buffer
                      (insert it)
                      (goto-char (point-min))
                      ;; remove all lines starting with #
                      (while (re-search-forward "^#.*\n?" nil t)
                        (replace-match ""))
                      (buffer-string))
                    (string-match (rx (not (any whitespace))) it))))
        (message "Commit buffer not empty. Discarding preparations.")
      (let ((subject nil)
             ;; (--> (--read-word-list :prompt "Tags")
             ;;      (unless (string-empty-p it)
             ;;        (format "[%s]" it))))
            (jira (let* ((branch (magit-get-current-branch)))
                    (if-let* ((point (string-match --jira-regex branch)))
                        (match-string 0 branch)))))
        (insert (pcase-exhaustive (list subject jira)
                  (`(nil nil) "")
                  (`(,s nil) (concat s ": "))
                  (`(nil ,b) (concat b ": "))
                  (`(,s ,b) (concat s " " b ": ")))))))
  :hook ((git-commit-setup-hook . evil-insert-state)
         (git-commit-setup-hook . --prepare-git-commit-message)
         (git-commit-setup-hook . aggressive-fill-paragraph-mode)
         ;; markdown-mode (which gfm-mode triggers hooks for) turns on
         ;; visual-line-fill-column-mode. For git commits we do not want that,
         ;; preferring aggressive-fill-paragraph-mode instead.
         (git-commit-setup-hook . (lambda ()
                                    (when (fboundp 'visual-line-fill-column-mode)
                                      (visual-line-fill-column-mode -1))))
         ;; note that the ordering here is important: we want this function to
         ;; fire _after_ `markdown-mode' has set up the comment-start/end
         ;; variables.
         (git-commit-setup-hook . --update-git-commit-comment-info)
         ;; evil-markdown-mode should fire after markdown-mode
         ;; (git-commit-setup-hook . evil-markdown-mode)
         (git-commit-setup-hook . markdown-ts-mode)
         (magit-mode-hook . --magit-on-mode-save-buffers))
  :config
  (require 'git-commit)
  (define-advice magit-log-propertize-keywords
      (:override (_rev msg) handle-conventional-commits)
    "https://github.com/magit/magit/issues/4027#issuecomment-1372397053"
    (let ((boundary 0))
      (when (string-match "^\\(?:squash\\|fixup\\)! " msg boundary)
        (setq boundary (match-end 0))
        (magit--put-face (match-beginning 0) (1- boundary)
                         'magit-keyword-squash msg))
      (when magit-log-highlight-keywords
        ;; Case [...]
        (while (string-match "\\[[^[]*?]" msg boundary)
          (setq boundary (match-end 0))
          (magit--put-face (match-beginning 0) boundary
                           'magit-keyword msg))
        ;; Conventional commits
        (while (string-match "^\\(?:feat\\|fix\\|chore\\|docs\\|style\\|refactor\\|perf\\|test\\)\\(?:\\(?:[(].*[)]\\)\\|\\(?:!\\)\\)?:" msg boundary)
          (setq boundary (match-end 0))
          (magit--put-face (match-beginning 0) boundary
                           'magit-keyword msg)))
      msg)))

(use-package magit-bisect
  :ensure nil
  :after (magit config-theme)
  :custom-face
  (magit-bisect-bad
   ((default . (:foreground ,sol-red))))
  (magit-bisect-good
   ((default . (:foreground ,sol-green))))
  (magit-bisect-skip
   ((default . (:foreground ,sol-yellow)))))

(use-package magit-blame
  :ensure nil
  :after (magit config-theme)
  :custom-face
  (magit-blame-highlight
   ((default . (:foreground unspecified
                :background unspecified
                :inherit sol-superlight-background)))))

(use-package magit-sequence
  :ensure nil
  :after (magit config-theme)
  :custom-face
  (magit-sequence-head
   ((default . (:foreground ,sol-green))))
  (magit-sequence-drop
   ((default . (:foreground ,sol-red
                :strike-through t))))
  (magit-sequence-part
   ((default . (:foreground ,sol-yellow))))
  (magit-sequence-stop
   ((default . (:foreground ,sol-violet)))))

(use-package magit-section
  :ensure nil
  :after (magit config-theme)
  :custom-face
  (magit-section-heading
   ((default . (:foreground unspecified
                :underline t
                :inherit sol-foreground))
    (((type nil)) . (:inherit sol-strong-foreground))))
  (magit-section-heading-selection
   ((default . (:foreground unspecified
                :background unspecified
                :inherit region))))
  (magit-section-highlight
   ((default . (:background unspecified
                :bold t))
    (((type nil)) . (:bold nil
                     :inherit sol-superlight-background)))))

(use-package magit-log
  :ensure nil
  :after (magit config-theme)
  :custom-face
  (magit-log-graph
   ((default . (:foreground unspecified
                :inherit sol-foreground))))
  (magit-log-author
   ((default . (:foreground unspecified
                :inherit sol-foreground))))
  (magit-log-date
   ((default . (:foreground unspecified
                :inherit sol-foreground)))))

(use-package magit-reflog
  :ensure nil
  :after (config-theme magit)
  :custom-face
  (magit-reflog-amend
   ((default . (:foreground ,sol-magenta))))
  (magit-reflog-checkout
   ((default . (:foreground ,sol-blue))))
  (magit-reflog-cherry-pick
   ((default . (:foreground ,sol-green))))
  (magit-reflog-commit
   ((default . (:foreground ,sol-green))))
  (magit-reflog-merge
   ((default . (:foreground ,sol-green))))
  (magit-reflog-other
   ((default . (:foreground ,sol-cyan))))
  (magit-reflog-rebase
   ((default . (:foreground ,sol-magenta))))
  (magit-reflog-remote
   ((default . (:foreground ,sol-cyan))))
  (magit-reflog-reset
   ((default . (:foreground ,sol-red)))))

(use-package magit-diff
  :ensure nil
  :after (magit config-theme)
  :general
  (magit-diff-mode-map
   :states 'normal
   "[[" 'help-go-back
   "]]" 'help-go-forward)
  :custom-face
  ;; base
  (magit-diff-context ((default . (:foreground unspecified
                                   :inherit sol-superlight-background))))
  (magit-diff-context-highlight ((default . (:foreground unspecified
                                             :background unspecified
                                             :inherit (sol-superlight-background
                                                       magit-diff-context)))))
  ;; removed
  (magit-diff-removed ((default . (:background unspecified
                                   :foreground ,sol-red
                                   :inherit magit-diff-context))))
  (magit-diff-removed-highlight ((default . (:foreground unspecified
                                             :background unspecified
                                             :inherit (magit-diff-removed
                                                       magit-diff-context-highlight)))))
  (magit-diffstat-removed ((t . (:foreground ,sol-red))))
  ;; added
  (magit-diff-added
   ((default . (:foreground ,sol-green
                :background unspecified
                :inherit magit-diff-context))))
  (magit-diff-added-highlight
   ((default . (:foreground unspecified
                :background unspecified
                :inherit (magit-diff-added
                          magit-diff-context-highlight)))))
  (magit-diffstat-added
   ((t . (:foreground ,sol-green))))
  ;; (rebase) ours
  (magit-diff-our
   ((default . (:foreground ,sol-violet
                :extend t
                :inherit sol-superlight-background))))
  (magit-diff-our-highlight
   ((default . (:inherit magit-diff-our))))
  ;; (rebase) theirs
  (magit-diff-their
   ((default . (:foreground ,sol-yellow
                :extend t
                :inherit sol-superlight-background))))
  (magit-diff-their-highlight
   ((default . (:inherit magit-diff-their))))
  ;; (rebase) base
  (magit-diff-base
   ((default . (:foreground ,sol-blue
                :background unspecified
                :extend t
                :inherit sol-superlight-background))))
  (magit-diff-base-highlight
   ((default . (:foreground unspecified
                :background unspecified
                :inherit magit-diff-base))))
  ;; hunk
  (magit-diff-hunk-heading
   ((default . (:bold t
                :foreground unspecified
                :background unspecified
                :extend nil
                :inherit (sol-light-foreground)))))
  (magit-diff-hunk-heading-highlight
   ((default . (:foreground unspecified
                :background unspecified
                :inherit magit-diff-hunk-heading))))
  (magit-diff-hunk-heading-selection
   ((default . (:foreground unspecified
                :inherit magit-diff-hunk-heading-highlight))))
  ;; file
  (magit-diff-file-heading
   ((default . (:foreground unspecified
                :inherit sol-foreground
                :bold t))))
  (magit-diff-file-heading-highlight
   ((default . (:inherit (magit-diff-file-heading
                          sol-superlight-background)
                :bold t))))
  (magit-diff-file-heading-selection
   ((default . (:foreground unspecified
                :inherit magit-diff-file-heading-highlight))))
  (magit-diff-lines-heading
   ((default . (:foreground unspecified
                :background unspecified
                :inherit magit-diff-hunk-heading-highlight))))
  ;; conflict markers
  (magit-diff-conflict-heading
   ((default . (:foreground ,sol-cyan
                :extend t
                :inherit sol-superlight-background))))
  ;; revision (?)
  (magit-diff-revision-summary
   ((default . (:inherit sol-foreground)))))

(use-package consult-git-log-grep
  :ensure (:host github :repo "ghosty141/consult-git-log-grep")
  :commands (consult-git-log-grep)
  :after (magit consult)
  :init
  (evil-ex-define-cmd "fg" #'consult-git-log-grep)
  (evil-ex-define-cmd "gl" "fg")
  :config
  (set-default 'consult-git-log-grep-open-function #'magit-show-commit))

;; ;; Add section headings for submodule information
;; ;;
;; ;; Also remember that C-u (or g C-u last I checked) applies individual
;; ;; submodule commands to all submodules
;; ;; https://github.com/magit/magit/issues/2657#issuecomment-220851059
;; ;; Disabled on windows because magit is _slow_ there. Hopefully mac is faster.
;; ;; TODO: some way to conditionally toggle this.
;;
;; (when (not (eq system-type 'windows-nt))
;;   (magit-add-section-hook 'magit-status-sections-hook
;;                           'magit-insert-modules-unpulled-from-upstream
;;                           'magit-insert-unpulled-from-upstream)
;;   (magit-add-section-hook 'magit-status-sections-hook
;;                           'magit-insert-modules-unpulled-from-pushremote
;;                           'magit-insert-unpulled-from-upstream)
;;   (magit-add-section-hook 'magit-status-sections-hook
;;                           'magit-insert-modules-unpushed-to-upstream
;;                           'magit-insert-unpulled-from-upstream)
;;   (magit-add-section-hook 'magit-status-sections-hook
;;                           'magit-insert-modules-unpushed-to-pushremote
;;                           'magit-insert-unpulled-from-upstream)
;;   (magit-add-section-hook 'magit-status-sections-hook
;;                           'magit-insert-modules-overview
;;                           'magit-insert-unpulled-from-upstream))

;; TODO: jigger `magit-todos-keyword-suffix' to handle rust todo!()
;; macros
;; TODO: Somehow jigger `magit-todos-branch-list' to *only* show
;; branch todos instead of it being an appended section
(use-package magit-todos
  :after hl-todo
  :commands (magit-todos-list)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "tt" #'(lambda () (interactive)
                                 (require 'magit)
                                 (magit-todos-list))))
  :custom
  (magit-todos-keyword-suffix
   (rx (optional (0+ blank) "(" (1+ (not (any ")"))) ")" (0+ blank)) ": "))
  (magit-todos-ignore-case t)
  (magit-todos-nice
   (not (eq system-type 'windows-nt))
   "`nice' does not exist on windows")
  (magit-todos-branch-list t)
  (magit-todos-group-by '(magit-todos-item-first-path-component
                          magit-todos-item-filename)))
                          ;; magit-todos-item-keyword))
  ;; :hook)
  ;; (magit-status-mode-hook . magit-todos-mode))

(use-package magit-lfs
  :after magit)

(use-package git-link
  :commands (git-link
             git-link-commit
             git-link-homepage)
  :custom
  (git-link-open-in-browser nil)
  :init
  (with-eval-after-load 'magit
    (general-define-key
     :states 'normal
     :keymaps 'magit-mode-map
      "y x" #'git-link-commit
      "y X" #'(lambda () (interactive)
                (require 'git-link)
                (let ((browse-url-browser-function #'browse-url-default-browser)
                      (url (progn
                               (git-link-commit (git-link--select-remote))
                               (pop kill-ring))))
                  (browse-url url)))))
  ;; TODO (pangt): something like :byg but for git specifically
  (evil-ex-define-cmd "repo" #'git-link-homepage))
  ;; (evil-ex-define-cmd "repo" #'(lambda () (interactive)
  ;;                                (require 'git-link)
  ;;                                (let ((browse-url-browser-function #'browse-url-default-browser)
  ;;                                      (url (progn
  ;;                                               (git-link-homepage (git-link--select-remote))
  ;;                                               (pop kill-ring))))
  ;;                                  (browse-url url)))))
  ;; (evil-ex-define-cmd "pulls" #'(lambda () (interactive)
  ;;                                 (require 'git-link)
  ;;                                 (let ((url (progn
  ;;                                              (git-link-homepage (git-link--select-remote))
  ;;                                              (pop kill-ring)))
  ;;                                       (browse-url-browser-function #'browse-url-default-browser))
  ;;                                  (browse-url (concat url "/pulls"))))))

;; (use-package magit-blame-color-by-age
;;   :ensure (:host github :repo "jdtsmith/magit-blame-color-by-age")
;;   :hook (magit-blame-mode-hook . magit-blame-color-by-age-mode))

(use-package git-timemachine
  :ensure (:host github :repo "emacsmirror/git-timemachine" :branch "master")
  :commands git-timemachine
  :after (evil transient general)
  :general
  (git-timemachine-mode-map
   :states 'motion
   "[["  'git-timemachine-show-previous-revision
   "]]"  'git-timemachine-show-next-revision
   "M-k" 'git-timemachine-show-next-revision
   "M-j" 'git-timemachine-show-previous-revision
   "g ?" 'git-timemachine-help
   "RET" '--git-timemachine)
  :init
  (transient-define-prefix --git-timemachine ()
    [:description "Git Timemachine dispatcher"
     ["Configuration"
      ("b" "Blame" git-timemachine-blame)
      ("?" "Help" git-timemachine-help)]
     ["Display"
      ("RET" "Show current commit" git-timemachine-show-commit)
      ("c c" "Fuzzy find commit" git-timemachine-show-revision-fuzzy)
      ("c n" "Nearest commit" git-timemachine-show-nearest-revision)]
     ["Navigation"
      ("j" "Previous commit" git-timemachine-show-previous-revision :transient t)
      ("k" "Next commit" git-timemachine-show-next-revision :transient t)
      ("t" "Fuzzy find commit" git-timemachine-show-revision-fuzzy)
      ("g" "Nth commit from start" git-timemachine-show-nth-revision)
      ("h" "Nearest commit to X" git-timemachine-show-nearest-revision)]
     ["Capture"
      ("y" "Yank short hash" git-timemachine-kill-abbreviated-revision)
      ("Y" "Yank full hash" git-timemachine-kill-revision)]])
  (evil-ex-define-cmd "glt" #'git-timemachine))

(use-package abridge-diff
  :after magit
  :hook (magit-diff-mode-hook . abridge-diff-enable))

(use-package blamer
  :ensure (:host github :repo "Artawower/blamer.el")
  :after (magit config-theme)
  :custom
  (blamer-commit-formatter ": %s")
  (blamer-min-offset 5)
  (blamer-idle-time 1)
  (blamer-view 'overlay-right)
  :custom-face
  (blamer-face
   ((default . (:foreground unspecified
                :background unspecified
                :inherit sol-light-foreground))
    (((supports (:weight))) . (:weight extra-light))))
  :hook
  (prog-mode-hook . blamer-mode))

(use-package diff-hl
  :ensure (:host github :repo "dgutov/diff-hl")
  :after (dired config-theme)
  :custom
  (diff-hl-disable-on-remote t)
  :custom-face
  (diff-hl-change
   ((default . (:foreground unspecified
                :background unspecified
                :inherit (sol-superlight-foreground
                          sol-superlight-background)))))
  (diff-hl-insert
   ((default . (:foreground unspecified
                :inherit diff-hl-change))))
  (diff-hl-delete
   ((default . (:foreground unspecified
                :inherit diff-hl-change))))
  (diff-hl-reverted-chunk-highlight
   ((default . (:inherit diff-hl-change))))
  :hook
  (dired-mode-hook . diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'config-git)
