;; This file is a file that contains information that pertains to this local
;; machine. Its existence and the definition of the constants/variables inside
;; should not be assumed to guarantee exist

;; This file should be one of the first things that init.el loads, and hence
;; shouldn't depend on anything that init.el loads

;; Known keywords that the config currently uses are:
;; - `config-local-dropbox-folder' :: The path to the dropbox directory

;; (defconst config-local-syncthing-folder "/mnt/c/Users/pangt.ADS/Sync")
;; (defconst config-local-dropbox-folder "/mnt/c/Users/pangt.ADS/Dropbox")

;; fullscreen if you want it
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (toggle-frame-fullscreen)
;; (toggle-frame-maximized)

;; Pre-init local declarations go here

;; Sample configuration for org-capture

;; (with-eval-after-load 'org-capture
;;     (add-to-list 'org-capture-templates
;;                  `("ww" "Work Task"
;;                    entry
;;                    (file ,config-local-work-log-file)
;;                    ,(concat "* TODO %^{DESCRIPTION}\n"
;;                             ":PROPERTIES:\n"
;;                             ":CREATED:  %t\n"
;;                             ":END:\n"
;;                             "%?")
;;                    :prepend t))
;;     (add-to-list 'org-capture-templates
;;                  `("wj" "Jira Work Task"
;;                    entry
;;                    ;; (file+olp ,config-local-work-log-file "Tasks")
;;                    (file ,config-local-work-log-file)
;;                    ,(concat "* TODO [%^{JIRA}] %^{DESCRIPTION} %(org-set-tags \":jira:\")\n"
;;                             ":PROPERTIES:\n"
;;                             ":CREATED:  %t\n"
;;                             ":JIRA:     https://jira.autodesk.com/browse/%\\1\n"
;;                             ":END:\n\n"
;;                             "- What is the scope of work?\n"
;;                             "  - [ ] %\\2\n"
;;                             "- [ ] Update JIRA\n"
;;                             "\n%?")
;;                    :prepend t)))

;; (defconst config-local-dropbox-folder
;;   (directory-file-name "..."))

;; Post-init local declarations go here

;; (defun my-after-init-code ()
;;   "Things to do after the init files are loaded"
;; (with-eval-after-load 'org-agenda
;;   (add-to-list 'org-agenda-files config-local-work-log-file)
;;   (add-to-list 'org-agenda-files config-local-dropbox-todo-file)))
;;   )
