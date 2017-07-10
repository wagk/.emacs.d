;;; config-org.el --- org-mode configuration

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)
(require 'config-helm)

(defun -evil-org-new-item ()
  "Insert a new item if we're in normal mode."
  (interactive)
  (when (org-in-item-p)
    (end-of-line)
    (org-insert-item)
    (evil-append 1)))

(defun -evil-org-toggle-checkbox ()
  "If the list element has no checkbox, add one. Do nothing otherwise."
  (interactive)
  (if (not (org-at-item-checkbox-p))
      (save-excursion (org-toggle-checkbox '(4))) ;; Prefix arguments are WEIRD
    (org-toggle-checkbox)
    ))

(defmacro -evil-update-cursor-eol(func)
  (lambda ()
    (interactive)
    (func)
    (end-of-line)))

(defun -org-insert-heading()
  (interactive)
  (org-insert-heading)
  (evil-append-line 1))

(use-package org
  :ensure t
  :after evil
  :config
  (org-toggle-link-display)
  (evil-declare-key 'normal org-mode-map
    (kbd "RET")     '-evil-org-new-item
    (kbd "M-RET")   '-evil-org-insert-heading
    (kbd "S-SPC")   '-evil-org-toggle-checkbox
    (kbd "L")       'org-shiftright
    (kbd "H")       'org-shiftleft
    (kbd "K")       'org-shiftup
    (kbd "J")       'org-shiftdown
    (kbd "M-l")     'org-metaright
    (kbd "M-h")     'org-metaleft
    (kbd "M-k")     'org-metaup
    (kbd "M-j")     'org-metadown
    (kbd "M-L")     '(-evil-update-cursor-eol(org-shiftmetaright))
    (kbd "M-H")     '(-evil-update-cursor-eol(org-shiftmetaleft))
    (kbd "M-K")     '(-evil-update-cursor-eol(org-shiftmetaup))
    (kbd "M-L")     '(-evil-update-cursor-eol(org-shiftmetadown)))
  (evil-declare-key 'insert org-mode-map
    (kbd "S-RET")   '-evil-org-new-item
    (kbd "M-l")     'org-metaright
    (kbd "M-h")     'org-metaleft
    (kbd "M-k")     'org-metaup
    (kbd "M-j")     'org-metadown
    (kbd "M-L")     'org-shiftmetaright
    (kbd "M-H")     'org-shiftmetaleft
    (kbd "M-K")     'org-shiftmetaup
    (kbd "M-L")     'org-shiftmetadown))

(use-package helm-org-rifle
  :ensure t
  :after org helm)

(provide 'config-org)

;;; config-org ends here
