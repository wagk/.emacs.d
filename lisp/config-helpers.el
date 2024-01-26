;;; config-helpers -- Weird one-off helper functions

;;; Commentary:

;;; Code:

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

(cl-defun --thing-at-point-or-user-input ()
  "Prompt the user for input, defaulting to symbol at point if none."
  (require 'thingatpt)
  (let* ((thing (thing-at-point 'symbol))
         (prompt (concat "Grep for"
                         (when thing
                           (format " (default: %s)" thing))
                         ": "))
         (string (read-string prompt)))
    (if (string-empty-p string)
        thing
      string)))

(cl-defun --convert-to-buffer (buf)
  "Normalizes BUF into a buffer, returning the buffer value."
  (let ((buffer (cond
                 ((stringp buf) (find-file-noselect buf))
                 ((or (symbol-function buf)
                      (functionp buf)) (funcall buf))
                 ((bufferp buf) buf)
                 (t (user-error "Buf is neither string, buffer nor fn! It is %s" buf)))))
    (cl-assert (bufferp buffer))
    buffer))

(provide 'config-helpers)

;;; config-helpers.el ends here
