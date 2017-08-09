;;; config-journal.el --- journalling things

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-org)
(require 'config-evil)

(use-package org-journal
  :ensure t
  :config
  ;; use google drive if available
  (when (boundp '/g-drive-folder)
    (setq org-journal-dir (concat /g-drive-folder "/journal")))

  (setq org-journal-date-format "%d%m%Y"
        org-journal-file-format "%d%m%Y")
  (progn (require 'evil-leader)
         (evil-leader/set-key
           "d d" 'org-journal-new-entry))
  )

(provide 'config-journal)

;;; config-journal.el ends here
