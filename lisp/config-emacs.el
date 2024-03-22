(require 'use-package)
(require 'config-evil)

;; Catch-all config for emac variables
(use-package emacs
  :demand t
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  (history-delete-duplicates t)
  (history-length t)
  (version-control t)
  (delete-old-versions t)
  (backup-by-copying t)
  (vc-make-backup-files t)
  (kept-new-versions 20)
  (kept-old-versions 5)
  (fill-column 80)
  (ff-always-try-to-create nil)
  (blink-matching-paren-highlight-offscreen t)
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (if (boundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
  (window-divider-mode -1)
  (column-number-mode)
  (cl-defun --point-to-file-and-line-number ()
    (interactive)
    (require 'project)
    (let* ((buf (if (project-current nil)
                  (file-relative-name (buffer-file-name)
                                      (project-root (project-current)))
                  (buffer-file-name)))
           (info (concat buf ":"
                         (number-to-string (line-number-at-pos)))))
      (kill-new info)
      (message "%s" info)))

  (cl-defun --kill-buffer-path ()
    (interactive)
    (let ((name (or (buffer-file-name) dired-directory)))
      (pcase name
        ;; hack since dired-directory might be a list
        ((or 'nil (pred listp)) (message "Not a file"))
        (name (kill-new name)
              (message "%s" name)))))

  (setq backup-directory-alist
        `(("." . ,(file-name-concat (when (featurep 'no-littering)
                                      no-littering-etc-directory)
                                    "backups"))))

  (with-eval-after-load 'evil
    (evil-ex-define-cmd "byl" #'--point-to-file-and-line-number)
    (evil-ex-define-cmd "byf" #'--kill-buffer-path)))

(with-eval-after-load 'minibuffer
  (general-define-key
   :keymap 'minibuffer-mode-map
   "C-<return>" "RET"))

(use-package savehist
  :ensure nil
  :custom
  (savehist-save-minibuffer-history 1)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :config
  (savehist-mode 1))

(use-package dired
  :demand t
  :ensure nil
  :hook
  (dired-mode-hook . hl-line-mode)
  (dired-mode-hook . dired-hide-details-mode)
  :general
  (dired-mode-map
   :states 'normal
   "<SPC>" nil                     ; was shadowing leader key bindings
   "SPC" nil                       ; was shadowing leader key bindings
   "-" 'dired-up-directory
   "d" 'dired-create-directory
   "e" 'dired-toggle-read-only ; similar interface to wgrep
   "i" nil ; unbind the original binding
   "Y" #'(lambda () (interactive)
           (dired-copy-filename-as-kill 0)))
  (dired-mode-map
   :states 'normal
   :prefix my-default-evil-leader-key
   "<SPC>" 'execute-extended-command)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "Ex[plore]" 'dired-jump)
    (evil-ex-define-cmd "Sex[plore]" #'(lambda () (interactive)
                                         (call-interactively 'evil-window-split)
                                         (dired-jump)))
    (evil-ex-define-cmd "Vex[plore]" #'(lambda () (interactive)
                                         (call-interactively 'evil-window-vsplit)
                                         (dired-jump)))
    (evil-ex-define-cmd "Tex[plore]" #'(lambda () (interactive)
                                         (if (>= emacs-major-version 27)
                                             (tab-bar-new-tab)
                                           (my-evil-new-tab nil))
                                         (dired-jump)))))

(use-package dired-auto-readme
  :disabled t
  :after dired
  :ensure (:host github :repo "amno1/dired-auto-readme")
  :commands (dired-auto-readme-mode)
  :general
  (dired-mode-map
   :states 'normal
   ")" '--toggle-dired-auto-readme-mode)
  :init
  (cl-defun --toggle-dired-auto-readme-mode ()
    (interactive)
    (if dired-auto-readme-mode
        (dired-auto-readme-mode -1)
      (dired-auto-readme-mode)))
  (add-hook 'dired-mode-hook 'dired-auto-readme-mode 99)
  :config
  (with-eval-after-load 'consult
    (define-advice consult-imenu (:around (func) toggle-dired-auto-readme-mode)
      (dired-auto-readme-mode -1)
      (condition-case err
          (funcall func)
        (t (dired-auto-readme-mode 1))
        (:success (dired-auto-readme-mode 1))))))

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t))

(use-package dired-imenu
  :after dired
  :init
  (add-hook 'dired-mode-hook
            #'(lambda ()
                (add-hook 'imenu-after-jump-hook #'dired-find-file nil t))))

(use-package flymake
  :ensure nil
  :custom
  (flymake-error-bitmap nil)
  (flymake-warning-bitmap nil)
  (flymake-note-bitmap nil))

(use-package dired-git-info
  :disabled t
  :ensure (:host github :repo "clemera/dired-git-info")
  :after dired
  :hook
  (dired-after-readin-hook . (lambda () (dired-git-info-auto-enable))))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items nil)
  :config
  (recentf-mode)
  (with-eval-after-load 'consult
    (evil-ex-define-cmd "recent" 'consult-recent-file)))

(use-package autoinsert
  :ensure nil
  :config
  (auto-insert-mode))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-fringe-mark nil)
  (bookmark-save-flag 1 "Write to bookmark file immediately")
  :custom-face
  (bookmark-face ((t (:inherit sol-subtle))))
  :config
  (defun config-define-bookmark (name path &optional overwrite annotation)
    "Programmatically creates and stores bookmarks into the bookmark file. We do
   this here because as of 2019-04-01T16:13:14+0800 we have no idea if there is an
   existing interface to do this. If one is found this will be marked obsolete and
   we'll move to that instead.

   The bookmark list format is found at `bookmark-alist'.

   NAME - Name of the bookmark. PATH - filepath of the bookmark. OVERWRITE - if
   true, overwrite an existing bookmark of the same name if one currently exists.
   ANNOTATION - Optional annotation of the bookmark.

   If PATH does not point to anywhere valid, this function is a no-op and no
   bookmark will be created."
    (require 'bookmark)
    (when (file-exists-p path)
      (let* ((annot (if annotation annotation ""))
             (alist `((filename . ,path)
                      (front-context-string . "")
                      (rear-context-string . "")
                      (position . 0)
                      (annotation . ,annot))))
        (bookmark-store name alist overwrite))))
  (config-define-bookmark "init" user-init-file)
  (config-define-bookmark "config" user-config-file)
  (config-define-bookmark "local" user-local-file))

(use-package diff
  :ensure nil
  :config
  (with-eval-after-load 'config-theme
    (set-face-attribute 'diff-changed-unspecified nil
                         :background 'unspecified
                         :inherit '(diff-changed sol-superlight-background))
    (set-face-attribute 'diff-error nil
                        :background 'unspecified
                        :foreground sol-red)
    (set-face-attribute 'diff-indicator-added nil
                        :foreground 'unspecified)
    (set-face-attribute 'diff-indicator-changed nil
                        :foreground 'unspecified)
    (set-face-attribute 'diff-indicator-removed nil
                        :foreground 'unspecified)))

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-window-plain))

(use-package calendar
  :ensure nil
  :custom
  (calendar-week-start-day 1 "monday"))

(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t))

(use-package tab-bar
  :ensure nil
  :general
  (evil-window-map
   ;; single window in tab gets moved into frame
   "g f" #'(lambda ()
             (interactive)
             (unless (= 1 (length (window-list)))
               (tab-window-detach))
             (tab-detach))
   "g w" "g f"
   ;; entire tab gets moved into frame
   "g F" 'tab-detach
   "g W" "g F")
  :custom
  (tab-bar-close-last-tab-choice 'delete-frame)
  (tab-bar-new-tab-choice t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-close-tab-select 'left)
  (tab-bar-new-button nil)
  (tab-bar-new-tab-to 'right)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "gt" 'tab-bar-switch-to-next-tab)
    (evil-ex-define-cmd "gT" 'tab-bar-switch-to-prev-tab)
    (evil-define-command my-tab-bar-tab-edit (file)
      (interactive "<f>")
      (let ((tab-bar-new-tab-choice (if file file "*scratch*")))
        (tab-bar-new-tab)))
    (evil-ex-define-cmd "tabn[ew]" 'my-tab-bar-tab-edit)
    (evil-ex-define-cmd "tabe[dit]" 'tab-bar-new-tab)
    (evil-ex-define-cmd "tabc[lose]" 'tab-bar-close-tab)
    (evil-define-command --tab-bar-rename-tab (name)
      (interactive "<a>")
      (tab-bar-rename-tab name))
    (evil-ex-define-cmd "tabr[ename]" '--tab-bar-rename-tab)
    (evil-ex-define-cmd "tabs" 'tab-bar-select-tab-by-name)
    (evil-ex-define-cmd "tt" 'tab-bar-select-tab-by-name)
    (evil-ex-define-cmd "tabm[ove]+" 'tab-bar-move-tab)
    (evil-ex-define-cmd "tabm[ove]-" 'tab-bar-move-tab-right)
    (evil-ex-define-cmd "tabd[etach]" 'tab-detach))
  :config
  (tab-bar-mode)
  (defun --tab-bar-tab-name-fn ()
    (require 'project)
    (let ((buffer-name (-> (minibuffer-selected-window)
                           (window-buffer)
                           (buffer-name))))
      (if-let ((project-info (project-current)))
          (format "%s<%s>" buffer-name (project-root project-info))
        (format "%s" buffer-name))))
  ;; (customize-set-value 'tab-bar-tab-name-function #'--tab-bar-tab-name-fn)
  (customize-set-value 'tab-bar-tab-name-function #'tab-bar-tab-name-truncated)

  (define-advice delete-frame (:around (oldfun &rest _old_args)
                                       --tab-bar-delete-tab-or-emacs)
    (interactive)
    (let* ((tabs (find-if (lambda (elem) (eq 'tabs (car elem)))
                          (frame-parameters)))
           (num-tabs (length (cdr tabs))))
      (if (eq num-tabs 1)
          (call-interactively oldfun)
        (tab-bar-close-tab)))))

(provide 'config-emacs)
