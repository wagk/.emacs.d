;;; config-web-browsing.el ---

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package eww
  :config
  (require 'config-evil)

  ;; https://github.com/GriffinSchneider/emacs-config/blob/master/eww-customizations.el
  (defvar gcs-shr-width 110)

  ;; eww stupidly overrides shr-width before calling shr-insert-document to render a page. So,
  ;; un-override it.
  (defadvice shr-insert-document (around force-shr-width activate)
    (let ((shr-width (min (1- (window-width)) gcs-shr-width)))
      ad-do-it))

  (defun eww-increase-width ()
    (interactive)
    (make-local-variable 'gcs-shr-width)
    (setq gcs-shr-width  (+ 10 gcs-shr-width))
    (eww-reload))
  (define-key eww-mode-map (read-kbd-macro "+") 'eww-increase-width)

  (defun eww-decrease-width ()
    (interactive)
    (make-local-variable 'gcs-shr-width)
    (setq gcs-shr-width  (- gcs-shr-width 10))
    (eww-reload))
  (define-key eww-mode-map (read-kbd-macro "-") 'eww-decrease-width)

  ;; Use vim kebindings for searching
  (define-key eww-mode-map (read-kbd-macro "/") 'evil-search-forward)
  (define-key eww-mode-map (read-kbd-macro "?") 'evil-search-backward)
  (define-key eww-mode-map (read-kbd-macro "n") 'evil-search-next)
  (define-key eww-mode-map (read-kbd-macro "N") 'evil-search-previous)

  ;; Use vim keybindings for scrolling
  (define-key eww-mode-map (read-kbd-macro "j") 'evil-next-line)
  (define-key eww-mode-map (read-kbd-macro "k") 'evil-previous-line)
  ;; (define-key eww-mode-map (read-kbd-macro "C-j") (lambda () (interactive) (next-line 2) (scroll-up 2)))
  ;; (define-key eww-mode-map (read-kbd-macro "C-k") (lambda () (interactive) (scroll-down 2) (previous-line 2)))
  (define-key eww-mode-map (read-kbd-macro "d") 'evil-scroll-down)
  (define-key eww-mode-map (read-kbd-macro "u") 'evil-scroll-up)

  ;; Use sane keybindings for forward/back
  (evil-define-key 'normal 'eww-mode-map "H" 'eww-back-url)
  (evil-define-key 'normal 'eww-mode-map "L" 'eww-forward-url))

(provide 'config-web-browsing)

;;; config-web-browsing.el ends here
