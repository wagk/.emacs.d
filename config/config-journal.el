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

(use-package org-journal
  :ensure t
  :config
  ;; use google drive if available
  (when (boundp '/g-drive-folder)
    (setq org-journal-dir (concat /g-drive-folder "/journal")))

  (setq org-journal-file-format "%d%m%Y")
  (progn (require 'evil-leader)
         (evil-leader/set-key
           "d d" '/new-org-journal-entry))
  )

(provide 'config-journal)

;;; config-journal.el ends here
