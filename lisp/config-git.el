(require 'use-package)
(require 'config-colors)

(use-package transient
  :straight (:host github :repo "magit/transient"))

;; If magit complains about not finding the config on windows, it's
;; because of [this issue], the easiest solution is to make a link.
;;
;; `mklink %APPDATA%\.gitconfig %USERPROFILE%\.gitconfig`
;;
;; [this issue]: https://github.com/magit/magit/issues/1497
(use-package magit
  :straight (:host github :repo "magit/magit" :branch "main")
  :after transient
  :commands (magit-status
             magit-pull
             magit-commit)
  :defer 15
  :custom
  (magit-prefer-remote-upstream t)
  (magit-blame-echo-style 'headings)
  (magit-blame-read-only t)
  (magit-log-auto-more t)
  :custom-face
  (magit-section-heading ((((background light)) (:foreground ,sol-yellow :underline t))
                          (((background dark)) (:foreground, sol-yellow :underline t))))
  :general
  ;; (general-define-key
  ;;   :keymaps 'project-prefix-map
  ;;   "v" 'magit-status)
  (magit-mode-map
   :states '(normal)
   "g x" 'magit-browse-thing)
  :init
  (evil-define-command ex-magit-cli (cmd)
    "Calls specific magit functions"
    (interactive "<a>")
    (cond
     ((eq cmd nil) (magit-status))
     (t (magit-shell-command (concat "git " cmd)))))
  (evil-ex-define-cmd "gF"     'magit-pull)
  (evil-ex-define-cmd "gB"     'magit-branch)
  (evil-ex-define-cmd "gb"     'magit-blame-addition)
  (evil-ex-define-cmd "blame"  'magit-blame-addition)
  (evil-ex-define-cmd "gblame" 'magit-blame)
  (evil-ex-define-cmd "gc"     'magit-commit)
  (evil-ex-define-cmd "gf"     'magit-fetch)
  (evil-ex-define-cmd "gg"     'ex-magit-cli)
  (evil-ex-define-cmd "git"    'ex-magit-cli)
  (evil-ex-define-cmd "gl"     'magit-log)
  (evil-ex-define-cmd "gp"     'magit-push)
  (evil-ex-define-cmd "gz"     'magit-stash)
  :hook ((git-commit-setup-hook . aggressive-fill-paragraph-mode)
         (git-commit-setup-hook . markdown-mode)
         (git-commit-setup-hook . evil-markdown-mode)
         (git-commit-setup-hook . (lambda () (display-fill-column-indicator-mode 1))))
  :config
  (add-to-list 'evil-motion-state-modes 'magit-mode)

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
  ;;   		                   'magit-insert-unpulled-from-upstream))

  (with-eval-after-load 'magit-diff
    (general-define-key
     :keymaps 'magit-diff-mode-map
     :states 'normal
     "[[" 'help-go-back
     "]]" 'help-go-forward)))

;; TODO: jigger `magit-todos-keyword-suffix' to handle rust todo!()
;; macros
;; TODO: Somehow jigger `magit-todos-branch-list' to *only* show
;; branch todos instead of it being an appended section
(use-package magit-todos
  :straight (:host github :repo "alphapapa/magit-todos")
  :custom
  (magit-todos-ignore-case t)
  (magit-todos-nice
   (not (eq system-type 'windows-nt))
   "`nice' does not exist on windows")
  (magit-todos-branch-list t)
  (magit-todos-group-by '(magit-todos-item-first-path-component
                          magit-todos-item-filename))
                          ;; magit-todos-item-keyword))
  :general
  (:keymaps
   '(magit-todos-section-map magit-todos-item-section-map)
   "jT" nil
   "jl" nil
   "j" nil)
  :commands
  (magit-todos-list)
  :init
  (evil-ex-define-cmd "gtodo" 'magit-todos-list)
  :hook
  (magit-status-mode-hook . magit-todos-mode))

(use-package git-link
  :straight t
  :commands (git-link
             git-link-commit
             git-link-homepage)
  :custom
  (git-link-open-in-browser nil)
  :init
  (evil-ex-define-cmd "repo" #'(lambda () (interactive)
                                 (require 'git-link)
                                 (let ((browse-url-browser-function #'browse-url-default-browser)
                                       (url (progn
                                                (git-link-homepage (git-link--select-remote))
                                                (pop kill-ring))))
                                   (browse-url url))))
  (evil-ex-define-cmd "pulls" #'(lambda () (interactive)
                                  (require 'git-link)
                                  (let ((url (progn
                                               (git-link-homepage (git-link--select-remote))
                                               (pop kill-ring)))
                                        (browse-url-browser-function #'browse-url-default-browser))
                                   (browse-url (concat url "/pulls"))))))

(use-package git-timemachine
  :straight t
  :commands git-timemachine
  :general
  (git-timemachine-mode-map
   :states 'normal
   "[[" 'git-timemachine-show-previous-revision
   "]]" 'git-timemachine-show-next-revision
   "M-k" 'git-timemachine-show-next-revision
   "M-j" 'git-timemachine-show-previous-revision)
  :init
  (evil-ex-define-cmd "gtime" #'git-timemachine))

(use-package blamer
  :straight (:host github :repo "Artawower/blamer.el")
  :custom
  (blamer-commit-formatter ": %s")
  (blamer-min-offset 5)
  (blamer-idle-time 1)
  :custom-face
  (blamer-face ((((background light)) (:background ,sol-base3 :foreground ,sol-base2))
                (((background dark)) (:background ,sol-base03 :foreground ,sol-base02))))
  :hook
  (prog-mode-hook . blamer-mode))

(provide 'config-git)
