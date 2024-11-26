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
  :after (elpaca evil transient general)
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
  :general
  ;; (general-define-key
  ;;   :keymaps 'project-prefix-map
  ;;   "v" 'magit-status)
  (magit-mode-map
   :states '(normal)
   "g x" 'magit-browse-thing)
  :init
  (when (eq system-type 'windows-nt)
    ;; magit requires seq 2.24, windows only seems to have 2.23
    ;; lisp stolen from https://github.com/progfolio/elpaca/issues/216
    (cl-defun --elpaca-unload-seq (e)
      (and (featurep 'seq) (unload-feature 'seq t))
      (elpaca--continue-build e))
    (cl-defun --elpaca-build-seq ()
      (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                           elpaca--pre-built-steps
                         elpaca-build-steps))
              (list #'--elpaca-unload-seq #'elpaca--activate-package)))
    (use-package seq
      :ensure `(seq :build ,(--elpaca-build-seq))
      :demand t)
    (elpaca-wait))

  (cl-defun --magit-on-mode-save-buffers ()
    (save-some-buffers t #'save-some-buffers-root))

  ;; the *dispatch commands are probably the most important
  (evil-ex-define-cmd "gf"      'magit-file-dispatch)
  (evil-ex-define-cmd "gg"      'magit-dispatch)

  (evil-ex-define-cmd "gF"      'magit-pull)
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
         (git-commit-setup-hook . evil-markdown-mode)
         (git-commit-setup-hook . markdown-mode)
         (magit-mode-hook . --magit-on-mode-save-buffers))
  :config
  (with-eval-after-load 'transient
    (transient-replace-suffix 'magit-dispatch #'magit-status-quick
      (list "j" "Show status" #'magit-status))
    (transient-replace-suffix 'magit-dispatch #'magit-push
      (list "p" "Push" #'magit-push)))
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

(use-package git-commit
  :ensure t
  :after magit)

(use-package magit-diff
  :ensure nil
  :after (magit general)
  :general
  (magit-diff-mode-map
   :states 'normal
   "[[" 'help-go-back
   "]]" 'help-go-forward))

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
  :ensure t
  :after (magit hl-todo)
  :custom
  (magit-todos-keyword-suffix
   (rx (optional (0+ blank) "(" (1+ (not (any ")"))) ")" (0+ blank)) ": "))
  (magit-todos-ignore-case t)
  (magit-todos-nice
   (not (eq system-type 'windows-nt))
   "`nice' does not exist on windows")
  (magit-todos-branch-list t)
  (magit-todos-group-by '(magit-todos-item-first-path-component
                          magit-todos-item-filename))
                          ;; magit-todos-item-keyword))
  :hook
  (magit-status-mode-hook . magit-todos-mode))

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

(use-package git-timemachine
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
  :after magit
  :custom
  (blamer-commit-formatter ": %s")
  (blamer-min-offset 5)
  (blamer-idle-time 1)
  (blamer-view 'overlay-right)
  :hook
  (prog-mode-hook . blamer-mode))

(use-package diff-hl
  :ensure t
  :after dired
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'config-git)
