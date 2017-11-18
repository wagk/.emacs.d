;;; config-journal.el --- journalling things

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-org)
(require 'config-evil)

(defun /new-org-journal-entry ()
  "Creates a org journal entry with a timestamp"
  (interactive)
  (org-journal-new-entry 't nil))

(use-package org-journal
  :init
  (evil-leader/set-key
    "d d" '/new-org-journal-entry)
  :config
  ;; use google drive if available
  (when (boundp '/g-drive-folder)
    (setq org-journal-dir (concat /g-drive-folder "/journal")))
  ;; make sure it enters in orgmode
  (add-hook 'org-journal-after-entry-create-hook
            'aggressive-fill-paragraph-mode)
  (setq org-journal-file-format "%d%m%Y")
  )

(provide 'config-journal)

;;; config-journal.el ends here
