;;; config-org.el --- org-mode configuration

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)
(require 'config-helm)
(require 'config-buffer)

;;;###autoload
(defun /org-insert-item-or-header-respect-content ()
  "Basically org-insert-heading-respect-content, except when you're on an item,
then insert a new item instead"
  (interactive)
  (cond ((org-at-item-p) (org-insert-item))
        (t (org-insert-heading-respect-content))))

;;;###autoload
(defun /evil-org-toggle-checkbox ()
  "If the list element has no checkbox, add one. Do nothing otherwise."
  (interactive)
  ;; (cond
  ;;  ((org-at-item-p)
  ;;   ))
  (if (not (org-at-item-checkbox-p))
      (save-excursion (org-toggle-checkbox '(4))) ;; Prefix arguments are WEIRD
    (org-toggle-checkbox))
    (end-of-line))

;;;###autoload
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

;;;###autoload
(defun /org-insert-heading()
  (interactive)
  (org-insert-heading)
  (evil-append-line 1))

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

(use-package org
  :init
  (general-define-key :prefix my-default-evil-leader-key
                      "O O" 'org-agenda
                      "o t" 'org-time-stamp
                      "o T" #'/this-time
                      "o o" 'org-capture
                      "o r" 'org-refile)
  :config
  (org-toggle-link-display)
  ;;Use google drive if available
  (when (boundp '/g-drive-folder)
    (setq org-directory (concat /g-drive-folder "/org")))

  (setq org-default-notes-file (concat org-directory "/TODO.org")
        org-M-RET-may-split-line '(default . nil)
        org-list-empty-line-terminates-plain-lists t
        org-enforce-todo-checkbox-dependencies     t
        org-enforce-todo-dependencies              t
        org-pretty-entities                        nil
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
  (add-to-list 'org-emphasis-alist '("`" org-code verbatim))
  ;; make it vim-compatitable
  (add-hook 'org-mode-hook '(lambda ()
                              (setq paragraph-start "\\|[     ]*$"
                                    paragraph-separate "[       ]*$")))
  (general-define-key :keymaps 'org-mode-map
                      :states '(normal insert)
                      "M-l"     'org-metaright
                      "M-h"     'org-metaleft
                      "M-k"     'org-metaup
                      "M-j"     'org-metadown
                      "S-SPC"   '/evil-org-toggle-checkbox
                      "C-M-RET" 'org-insert-subheading
                      "M-L"     '(/evil-update-cursor-eol(org-shiftmetaright))
                      "M-H"     '(/evil-update-cursor-eol(org-shiftmetaleft))
                      "M-K"     '(/evil-update-cursor-eol(org-shiftmetaup))
                      "M-L"     '(/evil-update-cursor-eol(org-shiftmetadown)))
  (general-define-key :keymaps 'org-mode-map
                      :states 'normal
                      "TAB"     'org-cycle
                      "z a"     'org-cycle
                      "L"       'org-shiftright
                      "H"       'org-shiftleft
                      "K"       'org-shiftup
                      "J"       'org-shiftdown)
  (general-define-key :keymaps 'org-mode-map
                      :states 'insert
                      "RET"     'newline-and-indent)

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
  (defun /this-time ()
    "Prints the time and date."
    (interactive)
    (org-time-stamp '(16) t))
  (progn (require 'fill-column-indicator)
         (add-hook 'org-mode-hook 'turn-on-fci-mode))
  (progn (require 'aggressive-fill-paragraph)
         (add-hook 'org-mode-hook #'aggressive-fill-paragraph-mode))
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

(use-package org-brain)

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
  :init
  (general-define-key :prefix my-default-evil-leader-key
                      "o O" 'helm-org-rifle)
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
  )

(provide 'config-org)

;;; config-org ends here
