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
  :config
  (column-number-mode)
  (cl-defun --point-to-file-and-line-number ()
    (interactive)
    (require 'project)
    (let* ((buf (if (project-current nil)
                  (file-relative-name (buffer-file-name)
                                      (project-root (project-current)))
                  (buffer-file-name)))
           (info (concat buf
                         ":"
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
  :after dired
  :ensure (:host github :repo "amno1/dired-auto-readme")
  :hook (dired-mode-hook . dired-auto-readme-mode))

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

(use-package calendar
  :ensure nil
  :custom
  (calendar-week-start-day 1 "monday"))

(provide 'config-emacs)
