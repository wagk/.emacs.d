;;; Obsidian-related helpers
;; We rely on Obsidian as our store of information. `obsidian-mode' can feel a
;; bit sus at times and we don't use all of its features anyway. Have a small
;; bootstrapped obsidian configuration we can use.

(defgroup 'config-obsidian nil
  "Personal Obsidian hacks")

(defcustom config-obsidian-directory nil
  "Obsidian directory"
  :type 'directory)

(cl-defun config-obsidian-find-file ()
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
    (find-file (file-name-concat config-obsidian-directory selected-file))))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "nn" #'config-obsidian-find-file))

(provide 'config-obsidian)
