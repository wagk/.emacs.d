(require 'use-package)

(use-package transient
  :straight (:host github :repo "magit/transient"))

(defun --transient--post-command-hack ()
  (transient--debug 'post-command)
  (transient--with-emergency-exit
    (cond
     ((and (eq (this-command-keys-vector) [])
           (= (minibuffer-depth)
              (1+ transient--minibuffer-depth)))
      (transient--suspend-override)
      (transient--delay-post-command (eq transient--exitp 'replace)))
     (transient--exitp
      (transient--post-exit))
     ;; somehow it's possible that `transient--prefix' is nil, which
     ;; causes the following `oref' to fail
     ((not transient--prefix)
      (transient--post-exit))
     ((eq this-command (oref transient--prefix command)))
     (t
      (let ((old transient--redisplay-map)
            (new (transient--make-redisplay-map)))
        (unless (equal old new)
          (transient--pop-keymap 'transient--redisplay-map)
          (setq transient--redisplay-map new)
          (transient--push-keymap 'transient--redisplay-map)))
      (transient--redisplay)))))

(advice-add 'transient--post-command :override
            #'--transient--post-command-hack)

;; If magit complains about not finding the config on windows, it's
;; because of [this issue], the easiest solution is to make a link.
;;
;; `mklink %APPDATA%\.gitconfig %USERPROFILE%\.gitconfig`
;;
;; [this issue]: https://github.com/magit/magit/issues/1497
(use-package magit
  :straight t
  :after transient
  :commands (magit-status
             magit-pull
             magit-commit)
  :defer 15
  :custom
  (magit-blame-echo-style 'lines)
  (magit-log-auto-more t)
  (magit-wip-mode nil
                  "It does get pretty laggy since we're making 3+
                  commits each change")
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
  (evil-ex-define-cmd "git" 'ex-magit-cli)
  (evil-ex-define-cmd "gg" 'ex-magit-cli)
  (evil-ex-define-cmd "gb" 'magit-branch)
  (evil-ex-define-cmd "gbl[ame]" 'magit-blame-echo)
  (evil-ex-define-cmd "gc" 'magit-commit)
  (evil-ex-define-cmd "gf" 'magit-fetch)
  (evil-ex-define-cmd "gF" 'magit-pull)
  (evil-ex-define-cmd "gp" 'magit-push)
  (evil-ex-define-cmd "gz" 'magit-stash)
  :hook ((git-commit-setup-hook . aggressive-fill-paragraph-mode)
         (git-commit-setup-hook . markdown-mode)
         (git-commit-setup-hook . evil-markdown-mode)
         (git-commit-setup-hook . (lambda () (display-fill-column-indicator-mode 1))))
  :config
  (add-to-list 'evil-motion-state-modes 'magit-mode)
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

(provide 'config::git)