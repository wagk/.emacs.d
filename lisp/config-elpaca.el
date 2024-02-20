;; -*- lexical-binding: nil -*-

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(add-hook 'after-init-hook #'elpaca-process-queues)

(when (eq system-type 'windows-nt)
  (setq elpaca-queue-limit 20)
  (elpaca-no-symlink-mode))

;; some issues with https://github.com/progfolio/elpaca/issues/143
;; (macroexpansion within let-bindings) that makes me hesitant to move this into
;; a function

(elpaca `(,@elpaca-order))

(elpaca-wait)

(if (not (boundp 'elpaca-ui-marks))
    (warn "elpaca-ui-marks can't be found! Skipping configuration")
  ;; Replace prefix emojis with non-emojis.
  (let ((subst-list '((elpaca-delete  . "D")
                      (elpaca-try     . "T")
                      (elpaca-rebuild . "R")
                      (elpaca-fetch   . "F")
                      (elpaca-merge   . "M")
                      (elpaca-pull    . "P"))))
    (mapcar (lambda (elem)
              (setf (plist-get (cdr elem) :prefix)
                    (or (cdr (assoc (car elem) subst-list)) "X")))
            elpaca-ui-marks)))

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (general-define-key
     :keymaps 'elpaca-ui-mode-map
     :states 'normal
      "M" 'elpaca-ui-mark-merge)))

(provide 'config-elpaca)
