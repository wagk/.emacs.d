;;; Obsidian-related helpers
;; We rely on Obsidian as our store of information. `obsidian-mode' can feel a
;; bit sus at times and we don't use all of its features anyway. Have a small
;; bootstrapped obsidian configuration we can use.

(defgroup 'config-obsidian nil
  "Personal Obsidian hacks")

(defcustom config-obsidian-directory nil
  "Location of Obsidian directory."
  :type 'directory)

(cl-defun config-obsidian--select-file-name ()
  "Search `config-obsidian-directory' for files ending in `.md'."
  (interactive)
  (require 'dash)
  (let* ((files (--> config-obsidian-directory
                     (directory-files-recursively it "\\.md$")
                     (-map #'(lambda (elem)
                               (file-relative-name elem
                                                   config-obsidian-directory))
                           it)))
         (selected-file (--completing-read "File: " files)))
    (file-name-concat config-obsidian-directory selected-file)))

(cl-defun config-obsidian-find-file ()
  "Opens a markdown file in `config-obsidian-directory'."
  (interactive)
  (find-file (config-obsidian--select-file-name)))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "nn" #'config-obsidian-find-file))

(cl-defun config-obsidian--find-file-and-point ()
  (config-obsidian-find-file)
  (condition-case err
      (consult-imenu)
    ;; no headings in file
    (t (goto-char (point-max)))
    (:success
     ;; ignore list items
     (if (markdown-list-item-at-point-p)
         (goto-char (point-max))
       ;; we want to append, so go to the next outline and backtrack
       (pcase (markdown-outline-next)
         ('nil (goto-char (point-max)))
         (_ (beginning-of-line)
            (newline-and-indent)
            (forward-line -1)))))))

(with-eval-after-load 'org-capture
  (require 'doct)
  (setq org-capture-templates
        (doct `(("Notes - File - Header"
                 :keys "c"
                 :type plain
                 :function config-obsidian--find-file-and-point
                 :unnarrowed t
                 :empty-lines 1
                 :template "# %<%F>\n\n%?"))))

  (evil-ex-define-cmd "nc" #'(lambda () (interactive) (org-capture nil "c"))))

(provide 'config-obsidian)
