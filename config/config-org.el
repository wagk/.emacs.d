;;; config-org.el --- org-mode configuration

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)
(require 'config-helm)
(require 'config-buffer)

(use-package org
  :commands
  (org-time-stamp-inactive
   org-capture
   org-refile
   org-agenda)
  :init
  (general-define-key :prefix my-default-evil-leader-key
                      "O O" 'org-agenda
                      "o t" 'org-time-stamp-inactive
                      "o T" #'my-time-stamp
                      "o o" 'org-capture
                      "o r" 'org-refile)
  :config

;; ;;;###autoload
;;   (defun /org-insert-item-or-header-respect-content ()
;;     "Basically org-insert-heading-respect-content, except when you're on an
;; item, then insert a new item instead"
;;     (interactive)
;;     (cond ((org-at-item-p) (org-insert-item))
;;           (t (org-insert-heading-respect-content))))

;; ;;;###autoload
;;   (defun my-org-add-checkbox ()
;;     "adds a checkbox if the cursor is on a list"
;;     (interactive)
;;     (when (org-at-item-p)
;;         (org-toggle-checkbox '(4))
;;         (evil-move-end-of-line)))

;; ;;;###autoload
;;   (defun /evil-org-toggle-checkbox ()
;;     "If the list element has no checkbox, add one. Do nothing otherwise."
;;     (interactive)
;;     ;; (cond
;;     ;;  ((org-at-item-p)
;;     ;;   ))
;;     (if (not (org-at-item-checkbox-p))
;;         (org-toggle-checkbox '(4)) ;; Prefix arguments are WEIRD
;;       (org-toggle-checkbox))
;;     ;; (end-of-line)
;;     )

  ;; ;;;###autoload
  ;;   (defun /org-toggle-checkbox-or-table-down (n)
  ;;     (interactive "p")
  ;;     (if (org-table-p)
  ;;         (org-table-copy-down n)
  ;;       (org-toggle-checkbox)))

  ;; (defmacro /evil-update-cursor-eol (func)
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (func)
  ;;     (org-end-of-line)))

;;;###autoload
  (defun /org-mode-face-no-resize ()
    "Stop the org-level headers from increasing in height relative to the other
text."
    (when (eq major-mode 'org-mode)
      (dolist (face '(org-level-1
                      org-level-2
                      org-level-3
                      org-level-4
                      org-level-5))
        (set-face-attribute face nil :weight 'semi-bold :height 1.0))))
  (add-hook 'org-mode-hook '/org-mode-face-no-resize)

  (org-toggle-link-display)
  ;;Use google drive if available
  ;; (when (boundp '/g-drive-folder)
  ;;   (setq org-directory (concat /g-drive-folder "/org")))

  ;; initialize org agenda things
  (add-to-list 'org-agenda-files my-org-directory)

  (setq org-startup-indented nil
        org-indent-mode-turns-on-hiding-stars nil
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window)

  (setq org-default-notes-file "~/TODO.org"
        org-M-RET-may-split-line '(default . nil)
        org-list-empty-line-terminates-plain-lists t
        org-enforce-todo-checkbox-dependencies     t
        org-enforce-todo-dependencies              t
        org-pretty-entities                        nil
        ;; org-insert-heading-respect-content t
        org-log-done                               'time
        org-log-redeadline                         'time
        org-log-reschedule                         'time
        org-blank-before-new-entry '((heading . auto)
                                     (plain-list-item . auto))
        org-refile-targets '((nil . (:maxlevel . 9)))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-highlight-latex-and-related '(latex))

  ;; when inserting a heading immediately go into insert mode
  (add-hook 'org-insert-heading-hook 'evil-insert-state)

  ;; (add-to-list 'org-emphasis-alist '("`" org-code verbatim))
  ;; make it vim-compatitable
  ;; (add-hook 'org-mode-hook '(lambda ()
  ;;                             (setq paragraph-start "\\|[     ]*$"
  ;;                                   paragraph-separate "[       ]*$")))
  (general-define-key :keymaps 'org-mode-map
                      :states '(normal insert)
                      "M-l"     'org-metaright
                      "M-h"     'org-metaleft
                      "M-k"     'org-metaup
                      "M-j"     'org-metadown
                      ;; "S-SPC"   '/evil-org-toggle-checkbox
                      ;; "C-M-return" 'org-insert-subheading ;; there a warning when this is turned on
                      "M-L"     'org-shiftmetaright
                      "M-H"     'org-shiftmetaleft
                      "M-K"     'org-shiftmetaup
                      "M-L"     'org-shiftmetadown)
  (general-define-key :keymaps 'org-mode-map
                      :states  'normal
                      "TAB"    'org-cycle
                      "<tab>"  'org-cycle
                      "z a"    'org-cycle
                      "L"      'org-shiftright
                      "H"      'org-shiftleft
                      "K"      'org-shiftup
                      "J"      'org-shiftdown)
  (general-define-key :keymaps 'org-mode-map
                      :states 'insert
                      "RET"     'newline-and-indent)

  ;; make smartparen autoskip "" because org-mode treats it as a string
  (require 'smartparens)
  (sp-local-pair 'org-mode "\"" nil :when '(:rem sp-in-string-p))

  ;; TODO: Figure out why sometimes when calling org-meta-return the cursor
  ;; positions are all out of whack

  ;; (advice-add 'org-meta-return :after #'evil-refresh-cursor)
  ;; (defun my-refresh-insert-cursor (&rest _)
  ;;   "docstring for my-refresh-insert-cursor"
  ;;   (interactive)
  ;;   ;; (evil-adjust-cursor t)
  ;;   ;; (evil-move-cursor-back t)
  ;;   (evil-refresh-cursor 'insert)
  ;;   )
  ;; (add-hook 'org-metareturn-hook #'my-refresh-insert-cursor)

  ;; org capture. https://github.com/syl20bnr/spacemacs/issues/5320
  (with-eval-after-load 'org-capture
    (define-key org-capture-mode-map [remap evil-save-and-close]
      'org-capture-finalize)
    (define-key org-capture-mode-map [remap evil-save-modified-and-close]
      'org-capture-finalize)
    (define-key org-capture-mode-map [remap evil-quit]
      'org-capture-kill))
  ;; (progn (require 'org-agenda)
  ;;        ;; TODO: rebind org-agenda keymaps
  ;;        )
  (defun my-time-stamp ()
    "Prints the time and date."
    (interactive)
    (org-time-stamp-inactive '(16)))

  (defun my-org-hook-configs ()
    "docstring for my-org-hook-configs"
    ;; NOTE: We turn this off because it is causing the cursor to do really
    ;; fucking weird things
    ;; (require 'fill-column-indicator)
    ;; (turn-on-fci-mode)
    (require 'aggressive-fill-paragraph)
    (aggressive-fill-paragraph-mode))
  (add-hook 'org-mode-hook #'my-org-hook-configs)
  )

;; ;; TODO: Figure out how to make this work
;; (unless (featurep 'org-wiki)
;;   (let ((url "https://raw.githubusercontent.com/caiorss/org-wiki/master/org-wiki.el"))
;;     (with-current-buffer (url-retrieve-synchronously url)
;;       (goto-char (point-min))
;;       (re-search-forward "^$")
;;       (delete-region (point) (point-min))
;;       (kill-whole-line)
;;       (package-install-from-buffer))))

(use-package org-brain
  :custom
  (org-brain-path my-wiki-directory
                  "Share the same path as deft.")
  (org-brain-file-entries-use-title nil
                                    "Speed optimisation since our filenames and
                                    title should match anyway")
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  (general-define-key :prefix my-default-evil-leader-key
                      "N" 'org-brain-visualize)
  ;; :config
  ;; (general-define-key :keymaps 'org-brain-visualize-mode-map
  ;;                     :states 'normal
  ;;                     "C-j" 'forward-button
  ;;                     "C-k" 'backward-button)
  )

(use-package org-radiobutton)

;; Export orgfiles as anki decks!
;; Looks great for jap study and just study in general
;; https://github.com/louietan/anki-editor
(use-package anki-editor)

(use-package evil-org
  :disabled t
  :after org
  :demand t
  :config
  (evil-org-set-key-theme '(textobjects
                            insert
                            navigation
                            additional
                            shift
                            todo
                            heading)))

(use-package worf)

(use-package helm-org-rifle
  :disabled t
  :general
  (:prefix my-default-evil-leader-key
           "o O" 'helm-org-rifle)
  :bind
  (:map helm-org-rifle-map
        ("C-w" . evil-delete-backward-word)
        ("\\"  . helm-select-action)
        ("C-j" . helm-next-line)
        ("C-k" . helm-previous-line)
        ("C-n" . helm-next-page)
        ("C-p" . helm-previous-page)
        ("C-l" . helm-next-source)
        ("C-h" . helm-previous-source)
        ("TAB" . helm-execute-persistent-action))
  )

(use-package ob-async
  :after (org))

(use-package ob-clojurescript
  :after (org))

(use-package ob-http
  :after (org))

(use-package ob-browser
  :after (org))

(use-package ob-restclient
  :after (org))

(use-package ob-rust
  :after (org))

(use-package ob-translate
  :after (org))

(provide 'config-org)

;;; config-org ends here
