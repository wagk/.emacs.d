(defgroup personal nil
  "A list of configuration variables that is needed from the local
  machine.")

(require 'config-prelude)

;; Load local configuration variables, we do it here so that
;; local.el gets access to the "core" init loads
(when (f-exists-p user-local-file)
  (load-file user-local-file))

(require 'config-helpers)
(require 'config-evil)
(require 'config-evil-helpers)
(require 'config-theme)
(require 'config-git)
(require 'config-completions)
(require 'config-emacs)
(require 'config-org)
(require 'config-org-capture)
(require 'config-org-capture-templates)

(require 'config-japanese)
(require 'config-anki)

(provide 'config)
