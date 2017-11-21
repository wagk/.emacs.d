;;; config-python.el --- python configuration file

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-prog)
(require 'config-completion)
(require 'config-lint)

;; ;; https://github.com/jorgenschaefer/elpy
;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable))

;; (add-hook 'python-mode-hook 'turn-on-ctags-auto-update-mode)
(add-hook 'python-mode-hook 'highlight-indent-guides-mode)

(use-package company-jedi
  :init
  (add-hook 'python-mode-hook #'(lambda ()
                                  (add-to-list 'company-backends 'company-jedi))))

(use-package flycheck-mypy
  :after flycheck
  :init
  (add-hook 'python-mode-hook #'(lambda ()
                                  (require 'flycheck)
                                  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
                                  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
                                  (add-to-list 'flycheck-disabled-checkers 'python-pycompile)
                                  (add-to-list 'flycheck-python-mypy-args "--ignore-missing-imports")
                                  (flycheck-mode))))

;; remove really dumb indentation rule when inside docstring
;; NOTE: it appears that :inside-docstring isn't documented
;; https://emacs.stackexchange.com/questions/26435/how-can-i-disable-indentation-rules-within-docstrings-in-python-mode
(when (and (>= emacs-major-version 25)
           (>= emacs-minor-version 1))
  (defun $python-mode-noindent-docstring (&optional _previous)
    (when (eq (car (python-indent-context)) :inside-docstring)
      'noindent))
  (advice-add 'python-indent-line :before-until #'$python-mode-noindent-docstring))

;; ;; https://github.com/jorgenschaefer/elpy/issues/498
;; (progn
;;   ;; Add python + rst major mode configuration.
;;   (defun rst-python-statement-is-docstring (begin)
;;     "Return true if beginning of statement is BEGIN."
;;     (save-excursion
;;       (save-match-data
;;         (python-nav-beginning-of-statement)
;;         (looking-at-p begin))))
;;   (defun rst-python-front-verify ()
;;     "Verify that we're looking at a python docstring."
;;     (rst-python-statement-is-docstring (match-string 0)))
;;   ;; (setq mmm-parse-when-idle 't)
;;   ;; (add-to-list 'mmm-save-local-variables 'adaptive-fill-regexp)
;;   ;; (add-to-list 'mmm-save-local-variables 'fill-paragraph-function)
;;   (mmm-add-classes
;;    '((rst-python-docstrings
;;       :submode rst-mode
;;       :face mmm-comment-submode-face
;;       :front "u?\\(\"\"\"\\|\'\'\'\\)"
;;       :front-verify rst-python-front-verify
;;       :back "~1"
;;       :end-not-begin t
;;       :save-matches 1
;;       :insert ((?d embdocstring nil @ "u\"\"\"" @ _ @ "\"\"\"" @))
;;       :delimiter-mode nil)))
;;   (mmm-add-mode-ext-class 'python-mode nil 'rst-python-docstrings)
;;   )

(provide 'config-python)

;;; config-python.el ends here
