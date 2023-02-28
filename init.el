;;; init.el --- Bootstrap further configurations

;;; Commentary:
;; Useful resources:
;; https://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
;; https://github.com/bbatsov/emacs-lisp-style-guide
;; https://github.com/noctuid/evil-guide

;; Latest builds can be found at:: alpha.gnu.org/gnu/emacs/pretest/windows/
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;;; Code:

;; Note that docstrings for variables come *after* the value

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defconst my-init-start-time (current-time))

(setq user-full-name    "Pang Tun Jiang"
      user-mail-address "mail@pangt.dev")

;; buffer encoding systems
;; We do this here because the package system might need to know our preferences
(customize-set-variable        'locale-coding-system 'utf-8)
(prefer-coding-system          'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-charset-priority          'unicode)
(set-clipboard-coding-system   'utf-8)
(set-default-coding-systems    'utf-8)
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(set-terminal-coding-system    'utf-8)

(defconst user-init-file
  (locate-user-emacs-file "init.el")
  "Points to init.el.")

(defconst user-config-file
  (locate-user-emacs-file "config.org")
  "Points to config.org.")

(defconst user-local-file
  (locate-user-emacs-file "local.el")
  "Points to local.el.")

(defconst user-variables-file
  (locate-user-emacs-file "lisp/config-variables.el")
  "Points to variables.el.")

(defun find-user-init-file ()
  "Edit `user-init-file' without opening a new window."
  (interactive)
  (find-file user-init-file))

(defun find-user-config-file ()
  "Edit `user-config-file' without opening a new window."
  (interactive)
  (find-file user-config-file))

(defun find-user-local-file ()
  "Edit `local.el' without opening a new window."
  (interactive)
  (find-file user-local-file))

(defun find-user-variables-file ()
  "Edit `variables.el' without opening a new window."
  (interactive)
  (find-file user-variables-file))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f seconds." (float-time (time-since time)))))

(defun bootstrap-package ()
  "Add package repositories and call `package-initialize'."
  (when package-enable-at-startup
    (require 'package)
    (dolist (x '(("melpa"        . "https://melpa.org/packages/")
                 ("melpa-2"      . "https://melpa.milkbox.net/packages/")
                 ("melpa-stable" . "https://stable.melpa.org/packages/")
                 ("elpy"         . "https://jorgenschaefer.github.io/packages/")
                 ("gnu"          . "https://elpa.gnu.org/packages/")
                 ("gnu-devel"    . "https://elpa.gnu.org/devel/")
                 ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                 ("marmalade"    . "https://marmalade-repo.org/packages/")))
      (add-to-list 'package-archives x)))
  (when (< emacs-major-version 27)
    ;; package-initialize doesn't have to be called here in emacs 27
    (package-initialize)))

(defun bootstrap-straight ()
  "Load straight.el, downloading it if necessary.
 `package-initialize' must be called prior to this."
  ;; Requires (package-initialize) to be called
  ;; https://github.com/raxod502/straight.el
  (customize-set-variable 'straight-repository-branch "develop")
  (customize-set-variable 'straight-use-package-by-default t)
  (customize-set-variable 'straight-check-for-modifications
                          '(check-on-save find-when-checking))
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el"
                           user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (message "Bootstrapping Straight.el...")
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (let ((lockfile (locate-user-emacs-file "packages.el"))
        (profile-name 'personal))
    (when (file-exists-p lockfile)
      (customize-set-variable 'straight-profiles
                              (add-to-list 'straight-profiles
                                           (cons profile-name
                                                 lockfile)))
      (straight-thaw-versions))
    (customize-set-variable 'straight-current-profile
                            profile-name)))

;;; Use package
(defun bootstrap-use-package ()
  "Check if use-package is installed and install it if it isn't.
 Then performs configuration of `use-package' variables."
  ;; (unless (featurep 'quelpa)
  ;;   (bootstrap-quelpa))
  ;; (quelpa
  ;;   '(quelpa-use-package
  ;;     :fetcher git
  ;;     :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
  ;; (require 'quelpa-use-package)
  (require 'straight)
  (customize-set-variable 'load-prefer-newer t)
  (straight-use-package '(use-package
                           :host github
                           :repo "jwiegley/use-package"))
  (require 'use-package)
  ;; download packages if needed
  (customize-set-variable 'use-package-always-defer nil
                          "we don't always lazy load because of explicitness")
  (customize-set-variable 'use-package-always-ensure nil
                          "always make sure it never skips if not
                          found. Disabled because we want straight to
                          do the heavy lifting")
  (customize-set-variable 'use-package-verbose t)
  (customize-set-variable 'use-package-compute-statistics t)
  (customize-set-variable 'use-package-hook-name-suffix nil)
  (use-package use-package-ensure-system-package))

(defun load-config-org-files (files)
  "Given a list of org FILES, load them sequentially in the order.
 specified The list of files is assumed to be relative to
 `user-init-dir' TODO: Error checking; relative pathing, error
 recovery. Maybe eventually load dependencies and all that."
  (dolist (file files)
    (message "Loading %s" file)
    (condition-case nil
        (org-babel-load-file (locate-user-emacs-file file))
      (error (message "There was an error when loading %s" file)))))

(defun my-init-solarized-color-variables-and-other-font-things ()
  "Solarized 1.0.0beta2[a] Color Palette[8]
 | Color   |    |     |     | sRGB    |     |     |     | xterm | Terminal  | Usage                          |
 |---------+----+-----+-----+---------+-----+-----+-----+-------+-----------+--------------------------------|
 | Name    | L* | a*  | b*  | Hex     |   R |   G |   B |  Code | Name      |                                |
 |---------+----+-----+-----+---------+-----+-----+-----+-------+-----------+--------------------------------|
 | Base03  | 15 | −12 | −12 | #002b36 |   0 |  43 |  54 |   234 | brblack   | background tones (dark theme)  |
 | Base02  | 20 | −12 | −12 | #073642 |   7 |  54 |  66 |   235 | black     | background tones (dark theme)  |
 | Base01  | 45 | −07 | −07 | #586e75 |  88 | 110 | 117 |   240 | brgreen   | content tones                  |
 | Base00  | 50 | −07 | −07 | #657b83 | 101 | 123 | 131 |   241 | bryellow  | content tones                  |
 | Base0   | 60 | −06 | −03 | #839496 | 131 | 148 | 150 |   244 | brblue    | content tones                  |
 | Base1   | 65 | −05 | −02 | #93a1a1 | 147 | 161 | 161 |   245 | brcyan    | content tones                  |
 | Base2   | 92 | −00 | 10  | #eee8d5 | 238 | 232 | 213 |   254 | white     | background tones (light theme) |
 | Base3   | 97 | 00  | 10  | #fdf6e3 | 253 | 246 | 227 |   230 | brwhite   | background tones (light theme) |
 | Yellow  | 60 | 10  | 65  | #b58900 | 181 | 137 |   0 |   136 | yellow    | accent tones                   |
 | Orange  | 50 | 50  | 55  | #cb4b16 | 203 |  75 |  22 |   166 | brred     | accent tones                   |
 | Red     | 50 | 65  | 45  | #dc322f | 220 |  50 |  47 |   160 | red       | accent tones                   |
 | Magenta | 50 | 65  | −05 | #d33682 | 211 |  54 | 130 |   125 | magenta   | accent tones                   |
 | Violet  | 50 | 15  | −45 | #6c71c4 | 108 | 113 | 196 |    61 | brmagenta | accent tones                   |
 | Blue    | 55 | −10 | −45 | #268bd2 |  38 | 139 | 210 |    33 | blue      | accent tones                   |
 | Cyan    | 60 | −35 | −05 | #2aa198 |  42 | 161 | 152 |    37 | cyan      | accent tones                   |
 | Green   | 60 | −20 | 65  | #859900 | 133 | 153 |   0 |    64 | green     | accent tones                   |"
  (dolist (col '((sol-base03  . "#002b36")
                 (sol-base02  . "#073642")
                 (sol-base01  . "#586e75")
                 (sol-base00  . "#657b83")
                 (sol-base0   . "#839496")
                 (sol-base1   . "#93a1a1")
                 (sol-base2   . "#eee8d5")
                 (sol-base3   . "#fdf6e3")
                 (sol-yellow  . "#b58900")
                 (sol-orange  . "#cb4b16")
                 (sol-red     . "#dc322f")
                 (sol-magenta . "#d33682")
                 (sol-violet  . "#6c71c4")
                 (sol-blue    . "#268bd2")
                 (sol-cyan    . "#2aa198")
                 (sol-green   . "#859900")))
    ;; TODO: set documentation string
    (set (car col) (cdr col)))

  ;; TODO: work on this
  (defface sol-critical '((t (:inherit default)))
    "Follows `nano-critical', for information that requires immediate action.
   It should be of high constrast when compared to other faces. This can
   be realized (for example) by setting an intense background color,
   typically a shade of red. It must be used scarcely.")

  (defface sol-popout '((t (:inherit default)))
    "Follows `nano-popout', for information that needs attention.
   To achieve such effect, the hue of the face has to be sufficiently
   different from other faces such that it attracts attention through the
   popout effect.")

  (defface sol-strong '((t (:inherit default)))
    "Follows `nano-strong', for information of a structural nature.
   It has to be the same color as the default color and only the weight
   differs by one level (e.g., light/regular or regular/bold). It is
   generally used for titles, keywords, directory, etc.")

  (defface sol-salient '((t (:inherit default)))
    "Follows `nano-salient', information that are important.
   To suggest the information is of the same nature but important, the
   face uses a different hue with approximately the same intensity as the
   default face. This is typically used for links.")

  (defface sol-faded '((t (:inherit default)))
    "Follows `nano-faded', Faded face is for information that are less important.
   It is made by using the same hue as the default but with a lesser
   intensity than the default. It can be used for comments, secondary
   information and also replace italic (which is generally abused
                                        anyway).")

  (defface sol-subtle `((((background light)) (:foreground ,sol-base1))
                        (((background dark)) (:foreground ,sol-base01)))
    "Follows `nano-subtle', to suggest a physical area on the screen.
   It is important to not disturb too strongly the reading of information
   and this can be made by setting a very light background color that is
   barely perceptible.")

  (defface sol-default '((t (:inherit default)))
    "Follows `nano-default'"))

(cl-defun --completing-read (prompt collection
                                    &key predicate require-match
                                    initial-input history default-value
                                    inherit-input-method)
  "Wrapper around `completing-read' that allow the use of keywords."
  (completing-read prompt collection
                   predicate
                   require-match
                   initial-input
                   history
                   default-value
                   inherit-input-method))

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(customize-set-variable 'straight-use-package-by-default t)

(bootstrap-package)
(bootstrap-use-package)

(my-init-solarized-color-variables-and-other-font-things)

;; Load core configuration that I can't work without. Everything
;; else gets shoved into config.org except these.

;; https://github.com/magnars/dash.el
(use-package dash :straight t)

;; https://github.com/plexus/a.el/
;; alist functions
(use-package a :straight t)

;; https://github.com/rejeep/f.el/
;; file functions
(use-package f :straight t)

;; https://github.com/magnars/s.el/
;; string functions
(use-package s :straight t)

;; https://github.com/Wilfred/ht.el/
;; hash-table functions
(use-package ht :straight t)

;; https://github.com/alphapapa/ts.el
;; date and time functions
(use-package ts :straight t)

(use-package async
  ;; :straight (:host github :repo "jwiegley/emacs-async")
  :straight t)

(use-package general
  :straight (:host github :repo "noctuid/general.el")
  :init
  (defconst my-default-evil-leader-key "SPC"))

(use-package no-littering
  :straight (:host github :repo "emacscollective/no-littering"))

(use-package restart-emacs
  :if (not (eq system-type 'darwin))
  :straight (:host github :repo "iqbalansari/restart-emacs")
  :commands (restart-emacs restart-emacs-start-new-emacs)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "restart" 'restart-emacs)
    (evil-ex-define-cmd "restarttest"
                        'restart-emacs-start-new-emacs)))

(let ((custom (locate-user-emacs-file "custom.el")))
  (unless (f-exists-p custom)
    (f-touch custom))
  (setq custom-file custom))

(add-to-list 'load-path (-> "lisp"
                            (locate-user-emacs-file)
                            (directory-file-name)))

(require 'config-variables)

;; Load local configuration variables, we do it here so that
;; local.el gets access to the "core" init loads
(when (f-exists-p user-local-file)
  (load-file user-local-file))

(customize-set-variable 'frame-background-mode 'nil)
(with-eval-after-load 'solarized-theme
  (load-theme --default-emacs-theme t))

(require 'config)
(org-babel-load-file (locate-user-emacs-file "config.org"))

(setq initial-scratch-message
      (s-join "\n"
              '("# Programmers are not to be measured by their ingenuity and their"
                "# logic but by the completeness of their case analysis.")))

(message "Loaded config.org in %.06f seconds."
          (float-time (time-since my-init-start-time)))

;; Emacs considers the following "dangerous" (i.e they'll ask you to
;; confirm)
(put 'list-timers 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'after-init-hook
          #'(lambda ()
              (message "Init in %.06f seconds."
                       (float-time (time-since my-init-start-time)))
              (message "Configuration complete.")))
