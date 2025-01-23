;; todo

;; TODO (pangt): Completion
;; TODO (pangt): Decide syntax

(defcustom config-todo-file "~/TODO"
  "TODO file"
  :group 'config-todo
  :type 'file)

(defvar-keymap config-todo-keymap
  "C-c C-c" #'config-todo-do
  "C-c C-s" #'(lambda ()
                (interactive)
                (require 'evil)
                (let ((sort-fold-case t))
                  (evil-ex-sort (point-min) (point-max)))))

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
  (add-hook 'post-command-hook #'config-todo--prefix-date-on-newline nil t)
  (use-local-map config-todo-keymap))

(with-eval-after-load 'config-evil-helpers
  (--evil-define-splits "dd" #'config-todo-open))

(provide 'config-todo)
