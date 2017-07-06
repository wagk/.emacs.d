;;; dependencies.el --- Headers and packages other packages need

;;; Commentary:
;; This should come after config/package.el because it needs use-package

;;; Code:
(with-no-warnings (require 'cl))
(use-package dash)
(use-package s)
