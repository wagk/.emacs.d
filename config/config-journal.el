;;; config-journal.el --- journalling things

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-org)
(require 'config-evil)

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir (concat org-directory "/journal/")
        org-journal-date-format "%d%m%Y")
  (progn (require 'evil-leader)
         (evil-leader/set-key
           "d d" 'org-journal-new-entry))
  )

(provide 'config-journal)

;;; config-journal.el ends here
