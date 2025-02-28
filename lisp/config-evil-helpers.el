;; (require 'config-evil)
;; Must be evaluated after evil is loaded!

(evil-ex-define-cmd "view" #'(lambda () (interactive) (read-only-mode 'toggle)))

(cl-defun --evil-define-splits (command func-or-string)
  "Given a function that finds the appropriate buffer sets :{v,s,t,w} ex-cmd
variants.

Note that this is _not_ like `evil-ex-define-cmd' in the sense that it accepts a
string as an alias. (Might be useful to consider as a feature in the future, but
not something supported right now)

\(COMMAND BUFFER-FN)"
  (require 'evil)
  (if (stringp func-or-string)
      (progn (evil-ex-define-cmd command func-or-string)
             (evil-ex-define-cmd (concat "S" command)
                                 (concat "S" func-or-string))
             (evil-ex-define-cmd (concat "V" command)
                                 (concat "V" func-or-string))
             (evil-ex-define-cmd (concat "T" command)
                                 (concat "T" func-or-string))
             (evil-ex-define-cmd (concat "W" command)
                                 (concat "W" func-or-string)))
    (evil-ex-define-cmd command
                        `(lambda () (interactive)
                          (command-execute ',func-or-string)))
    (evil-ex-define-cmd (concat "S" command)
                        `(lambda () (interactive)
                          (--evil-do-in-split ',func-or-string :split)))
    (evil-ex-define-cmd (concat "V" command)
                        `(lambda () (interactive)
                          (--evil-do-in-split ',func-or-string :vsplit)))
    (evil-ex-define-cmd (concat "T" command)
                        `(lambda () (interactive)
                          (--evil-do-in-tab ',func-or-string)))
    (evil-ex-define-cmd (concat "W" command)
                        `(lambda () (interactive)
                          (--evil-do-in-frame ',func-or-string)))))

(--evil-define-splits "ld" #'(lambda () (interactive)
                               (dired user-emacs-directory)))
(--evil-define-splits "init"        'find-user-init-file)
(--evil-define-splits "local"       'find-user-local-file)
(--evil-define-splits "buffers"     'ibuffer)
(--evil-define-splits "me[ssage]"   #'(lambda () (interactive)
                                        (switch-to-buffer "*Messages*")))
(--evil-define-splits "sc[ratch]" #'scratch-buffer)
(--evil-define-splits "snippets" #'(lambda () (interactive)
                                     (dired (locate-user-emacs-file "snippets"))))


(evil-ex-define-cmd "Wn" '(lambda () (interactive)
                            (select-frame-set-input-focus (next-frame))))
(evil-ex-define-cmd "Wp" '(lambda () (interactive)
                            (select-frame-set-input-focus (previous-frame))))

(evil-define-command --rename-buffer (name)
  (interactive "<a>")
  (if name
      (rename-buffer name)
    (call-interactively 'rename-buffer)))
(evil-ex-define-cmd "br[ename]" '--rename-buffer)

(cl-defun --evil-ex-define-cmds-splits-and-tabs
    (command buffer-fn &optional tab)
  "Does split and vsplit, and also tab. BUFFER-FN should return a buffer
   Try moving to `--evil-ex-define-buffer-cmds' instead."
  (require 'evil)
  (unless (stringp command)
    (warn "given command is not a string! Got %s" command)
    (cl-return))
  (evil-ex-define-cmd command
                      `(lambda () (interactive)
                        (funcall-interactively #',buffer-fn)))
  (let ((split-command-name (concat "S" command)))
    (evil-ex-define-cmd split-command-name
                        `(lambda () (interactive)
                          (call-interactively 'evil-window-split)
                          (funcall-interactively #',buffer-fn))))
  (let ((vsplit-command-name (concat "V" command)))
    (evil-ex-define-cmd vsplit-command-name
                        `(lambda () (interactive)
                          (call-interactively 'evil-window-vsplit)
                          (funcall-interactively #',buffer-fn))))
  (when tab
    (let ((new-tab-command-name (concat "T" command)))
      (evil-ex-define-cmd new-tab-command-name
                          `(lambda () (interactive)
                            (require 'tab-bar)
                            (let* ((buffer (cond
                                            ((functionp ,tab)
                                             (funcall-interactively ,tab))
                                            ((or (bufferp ,tab)
                                                 (stringp ,tab))
                                             ,tab)
                                            (t (current-buffer))))
                                   (tab-bar-new-tab-choice buffer))
                              (tab-bar-new-tab)))))))

(cl-defun --evil-ex-define-buffer-cmds
    (command buf &key no-split no-vsplit no-tab)
  "Create :s*, :v*, and :t* variants of COMMAND, as configured.
   - BUF should be a function that accepts no arguments and returns a
    buffer.
   - NO-SPLIT, NO-VSPLIT, and NO-TAB causes the corresponding cmds to not
    be created.

   Note that some edge cases aren't handled yet: I haven't thought of
   what to do when BUF is `\(dired ...\)' or similar.

   Might also have issues when a quoted function is passed in \(like
   `\'ibuffer'\)."
  (require 'evil)
  (unless (stringp command)
    (warn "Given command is not a string! Got %s" command)
    (cl-return))
  (evil-ex-define-cmd command
                      `(lambda () (interactive)
                        (let ((sel (,buf)))
                          (cl-assert (bufferp sel) :show-args)
                          (pop-to-buffer-same-window sel))))
  (unless no-split
    (evil-ex-define-cmd (concat "S" command)
                        `(lambda () (interactive)
                           (let ((sel (,buf)))
                             (cl-assert (bufferp sel) :show-args)
                             (call-interactively 'evil-window-split)
                             (pop-to-buffer-same-window sel)))))
  (unless no-vsplit
    (evil-ex-define-cmd (concat "V" command)
                        `(lambda () (interactive)
                           (let ((sel (,buf)))
                             (cl-assert (bufferp sel) :show-args)
                             (call-interactively 'evil-window-vsplit)
                             (pop-to-buffer-same-window sel)))))
  (unless no-tab
    (evil-ex-define-cmd (concat "T" command)
                        `(lambda () (interactive)
                          (require 'tab-bar)
                          (let* ((sel (,buf))
                                 (tab-bar-new-tab-choice
                                  (prog2
                                      (cl-assert (bufferp sel) :show-args)
                                      sel)))
                            (tab-bar-new-tab))))))

;; Looks weird because we attempt to collect the buffer name before
;; splitting the windows, in order to not have dangling windows if the
;; split is cancelled. `consult-buffer' doesn't return the buffer
;; itself so we have to find the buffer name by pretending to be
;; `consult--buffer-display', which gets called internally.
(cl-defun --evil-consult-buffer (split-type)
  (interactive)
  (let (selected-buffer)
    (cl-flet ((collect-buffer-name (buffer &optional _norecord)
                                   (setq selected-buffer buffer)))
      (let ((consult--buffer-display #'collect-buffer-name))
        (consult-buffer)
        (pcase split-type
          (:split
           (evil-window-split)
           (evil-buffer selected-buffer))
          (:vsplit
           (evil-window-vsplit)
           (evil-buffer selected-buffer))
          (:tab
           (require 'tab-bar)
           (let ((tab-bar-new-tab-choice selected-buffer))
             (tab-bar-new-tab))))))))

(evil-ex-define-cmd "vbb" #'(lambda () (interactive) (--evil-consult-buffer :vsplit)))
(evil-ex-define-cmd "sbb" #'(lambda () (interactive) (--evil-consult-buffer :split)))
(evil-ex-define-cmd "tbb" #'(lambda () (interactive) (--evil-consult-buffer :tab)))

(defcustom --additional-config-files
  (list user-init-file
        user-local-file
        (locate-user-emacs-file "early-init.el")
        (when-let*
            ((templates (locate-user-emacs-file "templates.eld"))
             (file-exists-p templates))
          templates))
  "Additional configuration files of interest not inside /lisp."
  :type '(list string))

(defun --select-config-lisp-file-name ()
  "Open a file from `.emacs.d/lisp'."
  (interactive)
  (require 'f)
  (require 'dash)
  (let* ((files (--> (directory-files user-lisp-dir)
                     (seq-filter #'(lambda (file)
                                     (-any (lambda (e) (f-ext-p file e))
                                           '("el" "org")))
                                 it)
                     (seq-filter #'(lambda (file)
                                     (not (string-match file-name-version-regexp
                                                        file)))
                                 it)
                     (seq-filter #'(lambda (file)
                                     (->> (file-name-base file)
                                          (string-prefix-p ".#")
                                          (not)))
                                 it)
                     (seq-filter #'(lambda (file)
                                     (->> (file-name-base file)
                                          (string-match "flycheck_")
                                          (not)))
                                 it)
                     (mapcar #'(lambda (file) (f-join user-lisp-dir file)) it)
                     (append it --additional-config-files)
                     (mapcar #'(lambda (file)
                                 (file-relative-name file user-emacs-directory))
                             it)))
         (file (--completing-read
                "file: " files
                :require-match t)))
    (concat user-emacs-directory file)))

(defun --select-config-lisp-file ()
  (interactive)
  (find-file (--select-config-lisp-file-name)))

(with-eval-after-load 'rg
  (rg-define-search --rg-search-lisp
    :files "elisp"
    :query ask
    :dir user-lisp-dir)
  (evil-ex-define-cmd "rconfig" #'--rg-search-lisp))

(cl-defun --select-lisp-config-root-file ()
  (interactive)
  (find-file (locate-user-emacs-file "lisp/config.el")))
(--evil-define-splits "config" #'--select-config-lisp-file)
(--evil-define-splits "cofnig" "config")
(--evil-define-splits "cnfig"  "config")

(defun --load-config-lisp-files (file-list)
  (cl-dolist (file file-list)
    (let ((file (locate-user-emacs-file file)))
      (pcase (file-name-extension file)
        ("el" (load-file file))
        ("org" (org-babel-load-file file))))))

(evil-define-command config-ex-set-arg (cmd)
  (interactive "<a>")
  (cond
    ((string= cmd "wrap") (visual-line-mode 1))
    ((string= cmd "nowrap") (visual-line-mode -1))
    ((string= cmd "fill") (progn (require 'visual-fill-column)
                                 (visual-line-fill-column-mode 1)))
    ((string= cmd "nofill") (progn (require 'visual-fill-column)
                                   (visual-line-fill-column-mode -1)))))
(evil-ex-define-cmd "set" 'config-ex-set-arg)

(evil-ex-define-cmd "date" #'(lambda () (interactive)
                               (insert (format-time-string "%F"))))
(evil-ex-define-cmd "today" "date")

(evil-ex-define-cmd "time" #'(lambda () (interactive)
                               (insert (format-time-string "%H:%M"))))

;; Or evil-spongebob if you want to think of it that way.
(evil-define-operator evil-studlify (beg end)
  (studlify-region beg end))
(general-define-key :keymaps '(normal visual)
  "g S" 'evil-studlify)

(provide 'config-evil-helpers)
