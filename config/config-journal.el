;;; config-journal.el --- journalling things

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-org)
(require 'config-evil)

(defun /new-org-journal-entry ()
  "Creates a org journal entry with a timestamp"
  (interactive)
  (org-journal-new-entry 't))

(defun /new-org-journal-entry-on-enter ()
  (org-mode)
  (aggressive-fill-paragraph-mode))

(use-package org-journal
  :ensure t
  :config
  ;; use google drive if available
  (when (boundp '/g-drive-folder)
    (setq org-journal-dir (concat /g-drive-folder "/journal")))

  ;; make sure it enters in orgmode
  (add-hook 'org-journal-after-entry-create-hook
            '/new-org-journal-entry-on-enter)

  (setq org-journal-file-format "%d%m%Y")
  (progn (require 'evil-leader)
         (evil-leader/set-key
           "d d" '/new-org-journal-entry))
  )

(provide 'config-journal)

;;; config-journal.el ends here
