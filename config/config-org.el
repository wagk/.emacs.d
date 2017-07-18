;;; config-org.el --- org-mode configuration

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)
(require 'config-helm)
(require 'config-buffer)

(defun /evil-org-new-item-or-header ()
  "Inserts a new item or a header if you are currently on an item or header.
Automatically puts you into insert mode."
  (interactive)
  (cond ((org-at-item-p) (progn (end-of-line)
                                (org-insert-item)
                                (evil-append 1)))
        ((org-at-heading-p) (progn (end-of-line)
                                   (org-insert-heading-respect-content)
                                   (evil-append 1)))))

(defun /evil-org-toggle-checkbox ()
  "If the list element has no checkbox, add one. Do nothing otherwise."
  (interactive)
  (if (not (org-at-item-checkbox-p))
      (save-excursion (org-toggle-checkbox '(4))) ;; Prefix arguments are WEIRD
    (org-toggle-checkbox)
    ))

(defmacro /evil-update-cursor-eol(func)
  (lambda ()
    (interactive)
    (func)
    (end-of-line)))

(defun /org-insert-heading()
  (interactive)
  (org-insert-heading)
  (evil-append-line 1))

(use-package org
  :ensure t
  :config
  (progn (org-toggle-link-display)
         (setq org-default-notes-file (concat org-directory "/TODO.org")
               org-M-RET-may-split-line '(default . nil)
               org-startup-indented t
               org-list-empty-line-terminates-plain-lists t)
         (add-to-list 'org-emphasis-alist '("`" org-code verbatim))
         (add-hook 'org-mode-hook '(lambda ()
                                     (setq paragraph-start "\\|[     ]*$"
                                           paragraph-separate "[       ]*$")))
         )
  (progn (require 'evil)
         (evil-declare-key    'normal org-mode-map
           (kbd "TAB")        'org-cycle
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
  (progn (require 'org-capture)
         (define-key org-capture-mode-map [remap evil-save-and-close]
           'org-capture-finalize)
         (define-key org-capture-mode-map [remap evil-save-modified-and-close]
           'org-capture-finalize)
         (define-key org-capture-mode-map [remap evil-quit]
           'org-capture-kill)
         )
  (progn (require 'evil-leader)
         (evil-leader/set-key
           "o t" 'org-time-stamp
           "o T" #'(lambda () (interactive) (org-time-stamp '(16) t))
           "o o" 'org-capture
           "o i" 'org-refile)
         )
  (progn (require 'fill-column-indicator)
         (add-hook 'org-mode-hook 'turn-on-fci-mode))
  )

(use-package helm-org-rifle
  :ensure t
  :config
  (progn (require 'evil-leader)
         (evil-leader/set-key
           "o -" 'helm-org-rifle))
  )

(provide 'config-org)

;;; config-org ends here
