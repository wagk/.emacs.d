;;; config-org.el --- org-mode configuration

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)
(require 'config-helm)
(require 'config-buffer)

(defun /org-insert-item-or-header-respect-content ()
  "Basically org-insert-heading-respect-content, except when you're on an item,
then insert a new item instead"
  (interactive)
  (cond ((org-at-item-p) (org-insert-item))
        (t (org-insert-heading-respect-content))))

(defun /evil-org-toggle-checkbox ()
  "If the list element has no checkbox, add one. Do nothing otherwise."
  (interactive)
  (if (not (org-at-item-checkbox-p))
      (save-excursion (org-toggle-checkbox '(4))) ;; Prefix arguments are WEIRD
    (org-toggle-checkbox)
    ))

(defun /org-toggle-checkbox-or-table-down (n)
  (interactive "p")
  (if (org-table-p)
      (org-table-copy-down n)
    (org-toggle-checkbox)))

(defmacro /evil-update-cursor-eol(func)
  (lambda ()
    (interactive)
    (func)
    (end-of-line)))

(defun /org-insert-heading()
  (interactive)
  (org-insert-heading)
  (evil-append-line 1))

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

(use-package org
  :ensure t
  :config
  (progn (org-toggle-link-display)

         ;;Use google drive if available
         (when (boundp '/g-drive-folder)
           (setq org-directory (concat /g-drive-folder "/org")))

         (setq org-default-notes-file (concat org-directory "/TODO.org")
               org-M-RET-may-split-line '(default . nil)
               org-list-empty-line-terminates-plain-lists t
               org-enforce-todo-checkbox-dependencies     t
               org-enforce-todo-dependencies              t
               org-pretty-entities                        t
               ;; org-insert-heading-respect-content t
               org-log-done                               'time
               org-log-redeadline                         'time
               org-log-reschedule                         'time
               org-blank-before-new-entry '((heading . t)
                                            (plain-list-item . auto))
               org-refile-targets '((nil . (:maxlevel . 9)))
               org-refile-use-outline-path t
               org-outline-path-complete-in-steps nil
               org-refile-allow-creating-parent-nodes 'confirm
               org-highlight-latex-and-related '(latex))

         (add-hook 'org-mode-hook '/org-mode-face-no-resize)
         ;; (add-to-list 'org-emphasis-alist '("`" org-code verbatim))
         ;; make it vim-compatitable
         (add-hook 'org-mode-hook '(lambda ()
                                     (setq paragraph-start "\\|[     ]*$"
                                           paragraph-separate "[       ]*$")))
         )
  (progn (require 'evil)
         (evil-declare-key    'normal org-mode-map
           (kbd "TAB")        'org-cycle
           (kbd "z a")        'org-cycle
           ;; (kbd "RET")        '/evil-org-new-item-or-header
           ;; [(shift return)]      '/evil-org-new-item-or-header
           ;; (kbd "S-RET")      '/evil-org-new-item-or-header
           ;; (kbd "S-<return>") '/evil-org-new-item-or-header
           (kbd "S-SPC")      '/evil-org-toggle-checkbox
           (kbd "L")          'org-shiftright
           (kbd "H")          'org-shiftleft
           (kbd "K")          'org-shiftup
           (kbd "J")          'org-shiftdown
           (kbd "M-l")        'org-metaright
           (kbd "M-h")        'org-metaleft
           (kbd "M-k")        'org-metaup
           (kbd "M-j")        'org-metadown
           (kbd "M-L")        '(/evil-update-cursor-eol(org-shiftmetaright))
           (kbd "M-H")        '(/evil-update-cursor-eol(org-shiftmetaleft))
           (kbd "M-K")        '(/evil-update-cursor-eol(org-shiftmetaup))
           (kbd "M-L")        '(/evil-update-cursor-eol(org-shiftmetadown)))
         (evil-declare-key    'insert org-mode-map
           (kbd "RET")        'newline-and-indent
           (kbd "M-l")        'org-metaright
           (kbd "M-h")        'org-metaleft
           (kbd "M-k")        'org-metaup
           (kbd "M-j")        'org-metadown
           (kbd "M-L")        'org-shiftmetaright
           (kbd "M-H")        'org-shiftmetaleft
           (kbd "M-K")        'org-shiftmetaup
           (kbd "M-L")        'org-shiftmetadown)
         )
  ;; org capture. https://github.com/syl20bnr/spacemacs/issues/5320
  (with-eval-after-load "org-capture"
    (define-key org-capture-mode-map [remap evil-save-and-close]
      'org-capture-finalize)
    (define-key org-capture-mode-map [remap evil-save-modified-and-close]
      'org-capture-finalize)
    (define-key org-capture-mode-map [remap evil-quit]
      'org-capture-kill))
  (progn (require 'org-agenda)
         ;; TODO: rebind org-agenda keymaps
         )
  (progn (require 'evil-leader)
         (defun /this-time ()
           "Prints the time and date."
           (interactive)
           (org-time-stamp '(16) t))
         (evil-leader/set-key
           "O O" 'org-agenda
           "o t" 'org-time-stamp
           "o T" #'/this-time
           "o o" 'org-capture
           "o i" 'org-refile)
         )
  (progn (require 'fill-column-indicator)
         (add-hook 'org-mode-hook 'turn-on-fci-mode))
  ;; (progn (require 'aggressive-fill-paragraph)
  ;;        (add-hook 'org-mode-hook #'aggressive-fill-paragraph-mode))
  )

(use-package worf
  :ensure t
  )

(use-package helm-org-rifle
  :ensure t
  :bind(:map helm-org-rifle-map
             ("C-w" . evil-delete-backward-word)
             ("\\"  . helm-select-action)
             ("C-j" . helm-next-line)
             ("C-k" . helm-previous-line)
             ("C-n" . helm-next-page)
             ("C-p" . helm-previous-page)
             ("C-l" . helm-next-source)
             ("C-h" . helm-previous-source)
             ("TAB" . helm-execute-persistent-action))
  :config
  (progn (require 'evil-leader)
         (evil-leader/set-key
           "o O" 'helm-org-rifle))
  )

(provide 'config-org)

;;; config-org ends here
