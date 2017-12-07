;;; helm-org-rifle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "helm-org-rifle" "helm-org-rifle.el" (23080
;;;;;;  45213 0 0))
;;; Generated autoloads from helm-org-rifle.el

(autoload 'helm-org-rifle-define-command "helm-org-rifle" "\
Define interactive helm-org-rifle command, which will run the appropriate hooks.
Helm will be called with vars in LET bound.

\(fn NAME ARGS DOCSTRING &key SOURCES (LET nil) (TRANSFORMER nil))" nil t)

(helm-org-rifle-define-command "" nil "This is my rifle.  There are many like it, but this one is mine.\n\nMy rifle is my best friend.  It is my life.  I must master it as I\nmust master my life.\n\nWithout me, my rifle is useless.  Without my rifle, I am\nuseless.  I must fire my rifle true.  I must shoot straighter than\nmy enemy who is trying to kill me.  I must shoot him before he\nshoots me.  I will...\n\nMy rifle and I know that what counts in war is not the rounds we\nfire, the noise of our burst, nor the smoke we make.  We know that\nit is the hits that count.  We will hit...\n\nMy rifle is human, even as I, because it is my life.  Thus, I will\nlearn it as a brother.  I will learn its weaknesses, its strength,\nits parts, its accessories, its sights and its barrel.  I will\nkeep my rifle clean and ready, even as I am clean and ready.  We\nwill become part of each other.  We will...\n\nBefore God, I swear this creed.  My rifle and I are the defenders\nof my country.  We are the masters of our enemy.  We are the\nsaviors of my life.\n\nSo be it, until victory is ours and there is no enemy, but\npeace!" :sources (helm-org-rifle-get-sources-for-open-buffers))

(helm-org-rifle-define-command "current-buffer" nil "Rifle through the current buffer." :sources (helm-org-rifle-get-source-for-buffer (current-buffer)))

(helm-org-rifle-define-command "files" (&optional files) "Rifle through FILES, where FILES is a list of paths to Org files.\nIf FILES is nil, prompt with `helm-read-file-name'.  All FILES\nare searched; they are not filtered with\n`helm-org-rifle-directories-filename-regexp'." :sources (--map (helm-org-rifle-get-source-for-file it) files) :let ((files (helm-org-rifle--listify (or files (helm-read-file-name "Files: " :marked-candidates t)))) (helm-candidate-separator " ") (helm-cleanup-hook (lambda nil (when helm-org-rifle-close-unopened-file-buffers (if (= 0 helm-exit-status) (let ((candidate-source (helm-attr 'name (helm-get-current-source)))) (dolist (source (helm-get-sources)) (unless (or (equal (helm-attr 'name source) candidate-source) (not (helm-attr 'new-buffer source))) (kill-buffer (helm-attr 'buffer source))))) (dolist (source (helm-get-sources)) (when (helm-attr 'new-buffer source) (kill-buffer (helm-attr 'buffer source))))))))))

(helm-org-rifle-define-command "sort-by-latest-timestamp" nil "Rifle through open buffers, sorted by latest timestamp." :transformer 'helm-org-rifle-transformer-sort-by-latest-timestamp :sources (helm-org-rifle-get-sources-for-open-buffers))

(helm-org-rifle-define-command "current-buffer-sort-by-latest-timestamp" nil "Rifle through the current buffer, sorted by latest timestamp." :transformer 'helm-org-rifle-transformer-sort-by-latest-timestamp :sources (helm-org-rifle-get-source-for-buffer (current-buffer)))

(autoload 'helm-org-rifle-agenda-files "helm-org-rifle" "\
Rifle through Org agenda files.

\(fn)" t nil)

(autoload 'helm-org-rifle-directories "helm-org-rifle" "\
Rifle through Org files in DIRECTORIES.
DIRECTORIES may be a string or list of strings.  If DIRECTORIES
is nil, prompt with `helm-read-file-name'.  With prefix or
TOGGLE-RECURSION non-nil, toggle recursion from the default.
Files in DIRECTORIES are filtered using
`helm-org-rifle-directories-filename-regexp'.

\(fn &optional DIRECTORIES TOGGLE-RECURSION)" t nil)

(autoload 'helm-org-rifle-org-directory "helm-org-rifle" "\
Rifle through Org files in `org-directory'.

\(fn)" t nil)

(autoload 'helm-org-rifle-define-occur-command "helm-org-rifle" "\
Define `helm-org-rifle-occur' command to search BUFFERS.

\(fn NAME ARGS DOCSTRING &key BUFFERS FILES DIRECTORIES PREFACE)" nil t)

(helm-org-rifle-define-occur-command nil nil "Search all Org buffers, showing results in an occur-like, persistent buffer." :buffers (--remove (string= helm-org-rifle-occur-results-buffer-name (buffer-name it)) (-select 'helm-org-rifle-buffer-visible-p (org-buffer-list nil t))))

(helm-org-rifle-define-occur-command "current-buffer" nil "Search current buffer, showing results in an occur-like, persistent buffer." :buffers (list (current-buffer)))

(helm-org-rifle-define-occur-command "directories" (&optional directories) "Search files in DIRECTORIES, showing results in an occur-like, persistent buffer.\nFiles are opened if necessary, and the resulting buffers are left open." :directories (or directories (helm-read-file-name "Directories: " :marked-candidates t)))

(helm-org-rifle-define-occur-command "files" (&optional files) "Search FILES, showing results in an occur-like, persistent buffer.\nFiles are opened if necessary, and the resulting buffers are left open." :files (or files (helm-read-file-name "Files: " :marked-candidates t)))

(helm-org-rifle-define-occur-command "agenda-files" nil "Search Org agenda files, showing results in an occur-like, persistent buffer.\nFiles are opened if necessary, and the resulting buffers are left open." :files (org-agenda-files))

(helm-org-rifle-define-occur-command "org-directory" nil "Search files in `org-directory', showing results in an occur-like, persistent buffer.\nFiles are opened if necessary, and the resulting buffers are left open." :directories (list org-directory))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-org-rifle-autoloads.el ends here
