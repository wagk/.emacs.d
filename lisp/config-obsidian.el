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

;; (cl-defun config-obsidian-kill-list-item ()
;;   (interactive)
;;   (pcase-let ((`(,begin ,end ,_))
;;               (markdown-cur-list-item-bounds))
;;     (message "%s %s" begin end)))

(cl-defun config-obsidian-find-file ()
  "Opens a markdown file in `config-obsidian-directory'."
  (interactive)
  (find-file (config-obsidian--select-file-name)))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "nn" #'config-obsidian-find-file))

(with-eval-after-load 'org-capture
  (require 'doct)
  ;; Define capture template
  (setq org-capture-templates
        (doct `(("Markdown Capture"
                 :keys "c"
                 :type plain
                 :file config-obsidian--select-file-name
                 :template "\n# %?"))))
  (evil-ex-define-cmd "nc" #'(lambda () (interactive) (org-capture nil "c"))))

(provide 'config-obsidian)
