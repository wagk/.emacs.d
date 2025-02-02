;; todo

;; TODO (pangt): Completion, keywords at least
;; TODO (pangt): Decide syntax
;; every line is a todo, or a done
;; `--'s separates the todo and the done context
;; keywords can be foo:bar or :bar or bar: or foo:bar:baz (?)
;; TODO (pangt): font-lock strikethrough for dones

(defcustom config-todo-file "~/TODO"
  "TODO file"
  :group 'config-todo
  :type 'file)

;; not sure if we'll need this
(defcustom config-todo-done-file "~/DONE"
  "DONE file"
  :group 'config-todo
  :type 'file)

(defconst config-todo-timestamp-rx
  (rx (= 4 (any digit)) "-" (any "0-1") (any digit) "-" (any "0-3") (any digit)))

(defvar-keymap config-todo-keymap
  "C-c C-c" #'config-todo-do
  "C-c C-s" #'config-todo-organize)

(cl-defun config-todo-organize ()
  "Normalize each line and then sort buffer."
  (interactive)
  (config-todo--normalize-every-line)
  (delete-duplicate-lines (point-min) (point-max) nil nil nil t)
  (config-todo--sort-todos))

(cl-defun config-todo-collect-todos ()
  "Collect every line in the buffer and store them in a list somewhere")

(cl-defun config-todo--sort-todos ()
  (let* ((sort-fold-case t)
         (done-rx (rx line-start "x"))
         (done-point))
    ;; sort lines in buffer
    (sort-lines nil (point-min) (point-max))
    ;; try to find a done task
    (setq done-point (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward done-rx nil t)
                         (match-beginning 0))))
    (if done-point
        ;; reverse sort done and not-done lines separately
        (progn
          (with-restriction (point-min) done-point
            (sort-lines t (point-min) (point-max)))
          (with-restriction done-point (point-max)
            (sort-lines t (point-min) (point-max))))
      ;; otherwise reverse the sort unilaterally
      (sort-lines t (point-min) (point-max)))))

(cl-defun config-todo--normalize-line (line)
  "Takes in a string that is a line, and returns a string that is a line."
  (let* ((line (string-trim line)))
    (if (string-match (rx line-start (optional "x ") (regexp config-todo-timestamp-rx) line-end)
                      line)
        nil line)))

(cl-defun config-todo--normalize-every-line ()
  ;; iterate through every line in the buffer.
  ;; TODO (pangt): I wonder how we would remove lines completely.
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
             (line (config-todo--normalize-line line)))
        (delete-region (line-beginning-position) (line-end-position))
        (when line (insert line)))
      (forward-line))))

(cl-defun config-todo--toggle-done ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "x ")
        (delete-char 2)
      (insert "x "))))

(cl-defun config-todo-do ()
  "toggles between todo and done states (an x)"
  (interactive)
  (save-excursion
    (cond
     ;; ((use-region-p) (progn
     ;;                   (message "range %s" evil-ex-range)
     ;;                   (goto-char (region-beginning))
     ;;                   (while (< (point) (region-end))
     ;;                     (config-todo--toggle-done)
     ;;                     (forward-line))
     ;;                   (deactivate-mark)))
     (t (config-todo--toggle-done)))))

(cl-defun config-todo--prefix-date-on-newline ()
  "Inserts a timestamp into every new line"
  (when (and (bolp) (eolp) (not (use-region-p)))
    (beginning-of-line)
    (insert (format-time-string "%F "))
    (end-of-line)))

(cl-defun config-todo-open ()
  "Goes to `config-todo-file'"
  (interactive)
  (find-file config-todo-file)
  (config-todo-mode))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "dd" #'config-todo-open))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html
(define-derived-mode config-todo-mode text-mode "config-todo"
  "line-based TODO text file"
  (use-local-map config-todo-keymap)
  (add-hook 'post-command-hook #'config-todo--prefix-date-on-newline nil t))


(provide 'config-todo)
