;;; This file is only to be run when bankruptcy is declared

(use-package evil
    :demand t
    :straight (:host github
               :repo "emacs-evil/evil"
               :branch "master"))

(use-package helm
             :demand t
             :general (:states  'normal
                                :prefix my-default-evil-leader-key
                                "<SPC>"  'helm-M-x))
