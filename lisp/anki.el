(require 'use-package)

(use-package org-anki
  :after org
  :commands (org-anki-sync-entry
             org-anki-update-all
             org-anki-sync-all
             org-anki-delete-entry
             org-anki-delete-all))
