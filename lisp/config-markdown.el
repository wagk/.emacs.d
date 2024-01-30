;;; Markdown-related helpers
;; We rely on Markdown as our store of information. `obsidian-mode' can feel a
;; bit sus at times and we don't use all of its features anyway. Have a small
;; bootstrapped markdown configuration we can use.

(defgroup config-markdown nil
  "Personal Markdown hacks")

(defcustom config-markdown-directory nil
  "Location of Markdown directory."
  :type 'directory)

(cl-defun config-markdown--select-file-name ()
  "Search `config-markdown-directory' for files ending in `.md'."
  (interactive)
  (require 'dash)
  (let* ((files (--> config-markdown-directory
                     (directory-files-recursively it "\\.md$")
                     (-map #'(lambda (elem)
                               (file-relative-name elem
                                                   config-markdown-directory))
                           it)))
         (selected-file (--completing-read "File: " files)))
    ;; make sure it's markdown
    (unless (s-ends-with-p ".md" selected-file)
      (setq selected-file (concat selected-file ".md")))
    (file-name-concat config-markdown-directory selected-file)))

(cl-defun config-markdown-find-file ()
  "Opens a markdown file in `config-markdown-directory'."
  (interactive)
  (find-file (config-markdown--select-file-name)))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "nn" #'config-markdown-find-file))

(cl-defun config-markdown--find-point ()
  "Assuming that we are in the appropriate capture file, find the capture
point."
  ;; ignore list items
  (if (markdown-list-item-at-point-p)
    (goto-char (point-max))
    ;; we want to append, so go to the next outline and backtrack
    (pcase (markdown-outline-next)
      ('nil (goto-char (point-max)))
      (_ (beginning-of-line)
         (newline-and-indent)
         (forward-line -1)))))

(cl-defun config-markdown--find-file-and-point ()
  (config-markdown-find-file)
  (condition-case err
      (consult-imenu)
    ;; no headings in file
    (t (goto-char (point-max)))
    (:success (config-markdown--find-point))))

(with-eval-after-load 'org-capture
  (require 'doct)
  (setq org-capture-templates
        (doct-add-to
         org-capture-templates
         `(("Notes - File - Header"
            :keys "file"
            :type plain
            :function config-markdown--find-file-and-point
            :unnarrowed t
            :empty-lines 1
            :after-finalize
            ,#'(lambda () (setq org-capture-last-stored-marker (make-marker)))
            :template "# %?")))))

(provide 'config-markdown)
