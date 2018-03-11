;;; config-vars.el ---

;;; Commentary:

;; The values are placed here so that all internal code should refer to these
;; variables instead of "local.el", so we can put all the checks in one place
;; and not make a mess of things

;;; Code:

(defconst my-org-directory
  (file-name-as-directory
   (if (boundp '/dropbox-folder)
       (concat (directory-file-name /dropbox-folder) "/org")
    "~/org"))
  "orgmode directory."
  )

(defconst my-notes-directory
  (file-name-as-directory
   (if (boundp '/dropbox-folder)
      (concat (directory-file-name /dropbox-folder) "/notes")
    "~/notes"))
  "Notes directory. Mostly used for `deft-directory'.
This is different from the org-directory because these documents are more for
snippets and smaller things"
  )

(defconst my-wiki-directory
  (file-name-as-directory
   (if (boundp '/dropbox-folder)
      (concat (directory-file-name /dropbox-folder) "/wiki")
    "~/wiki"))
  "Used for org-brain. This is basically the wiki directory"
  )



(provide 'config-vars)

;;; config-vars.el ends here
